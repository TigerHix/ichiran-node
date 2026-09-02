#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

import {
  ANALYZER_ANNOTATIONS_SECTION_ID,
  AnalyzerAnnotationNotLoadedError,
  AnalyzerAnnotationsReader,
  analyzerAnnotationsMemorySource
} from '../../core/src/analyzer-annotations.js';
import {
  PortableAnalyzer,
  type PortableAnalysisResult
} from '../../core/src/analyzer.js';
import { ANALYZER_SUPPORT_SECTION_ID, openAnalyzerSupport } from '../../core/src/analyzer-support.js';
import { MORPHOLOGY_SECTION_ID, openMorphology } from '../../core/src/morphology.js';
import { openPack } from '../../core/src/pack.js';
import { ROOT_PAYLOAD_SECTION_ID, openRootPayload } from '../../core/src/root-payload.js';
import { SURFACE_INDEX_SECTION_ID, openSurfaceIndex } from '../../core/src/surface-index.js';
import {
  loadAnalyzerParityCorpus,
  type AnalyzerFixtureRequest
} from '../../core/tools/parity-corpus.js';

interface CorpusEntity {
  readonly start: number;
  readonly end: number;
  readonly boost?: number;
}

interface CorpusRequest {
  readonly text: string;
  readonly limit: number;
  readonly entities?: readonly CorpusEntity[];
  readonly normalizePunctuation: boolean;
}

interface CorpusCase {
  readonly name: string;
  readonly request: CorpusRequest;
}

const QUALIFIED_HOT_SHA256 = '61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0';

function request(
  value: AnalyzerFixtureRequest,
  entities?: readonly CorpusEntity[]
): CorpusRequest {
  return {
    text: value.text,
    limit: value.limit,
    entities,
    normalizePunctuation: value.normalizePunctuation ?? true
  };
}

async function corpusCases(repository: string): Promise<CorpusCase[]> {
  const corpus = await loadAnalyzerParityCorpus(repository);
  const result: CorpusCase[] = [];
  const append = (suite: string, expected: number, requests: readonly CorpusRequest[]): void => {
    if (requests.length !== expected) {
      throw new Error(`${suite} has ${requests.length} C requests; expected ${expected}`);
    }
    requests.forEach((value, index) => result.push({ name: `${suite}:${index}`, request: value }));
  };
  append('segmentation', 534, corpus.segmentation.map(value => ({
    text: value.input,
    limit: 1,
    normalizePunctuation: false
  })));
  append('cli', 252, corpus.cli.map(value => request(value)));
  append('hard', 149, corpus.hard.map(value => request(value)));
  append('counters', 200, corpus.counters.map(value => request(value)));
  append('entities', 54, corpus.entities.map(value => request(
    { text: value.text, limit: 1 },
    value.entities
  )));
  append('probes', 47, corpus.probes.map(value => request(value.request)));
  if (result.length !== 1_236) {
    throw new Error(`C parity corpus has ${result.length} requests; expected 1236`);
  }
  return result;
}

function sourceRevision(repository: string): string {
  const git = Bun.spawnSync(['git', 'rev-parse', 'HEAD'], { cwd: repository });
  if (git.exitCode !== 0) throw new Error(new TextDecoder().decode(git.stderr));
  return new TextDecoder().decode(git.stdout).trim();
}

async function frozenAnalyzer(hot: Uint8Array): Promise<{
  analyze(value: CorpusRequest): Promise<PortableAnalysisResult>;
}> {
  const pack = openPack(hot);
  const annotations = await AnalyzerAnnotationsReader.open(
    analyzerAnnotationsMemorySource(pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID)),
    async (compressed, expectedByteLength) => {
      const decoded = new Uint8Array(gunzipSync(compressed));
      if (decoded.byteLength !== expectedByteLength) {
        throw new Error(
          `annotation block decoded to ${decoded.byteLength} bytes; expected ${expectedByteLength}`
        );
      }
      return decoded;
    }
  );
  const view = annotations.createPreloaded();
  const analyzer = new PortableAnalyzer({
    surface: openSurfaceIndex(pack.getSection(SURFACE_INDEX_SECTION_ID)),
    roots: openRootPayload(pack.getSection(ROOT_PAYLOAD_SECTION_ID)),
    morphology: openMorphology(pack.getSection(MORPHOLOGY_SECTION_ID)),
    support: openAnalyzerSupport(pack.getSection(ANALYZER_SUPPORT_SECTION_ID)),
    annotations: view
  });
  return {
    async analyze(value): Promise<PortableAnalysisResult> {
      const loaded = new Set<string>();
      try {
        for (;;) {
          try {
            return analyzer.analyze(value.text, {
              limit: value.limit,
              entities: value.entities,
              normalizePunctuation: value.normalizePunctuation
            });
          } catch (error) {
            if (!(error instanceof AnalyzerAnnotationNotLoadedError)) throw error;
            const key = `${error.kind}:${error.definitionSeq}`;
            if (loaded.has(key)) throw new Error(`${key} remained missing after preload`);
            loaded.add(key);
            await view.preloadMissing(error);
          }
        }
      } finally {
        view.clear();
      }
    }
  };
}

function utf16Hex(value: string): string {
  const units = new Array<string>(value.length);
  for (let index = 0; index < value.length; index++) {
    units[index] = value.charCodeAt(index).toString(16).padStart(4, '0');
  }
  return units.join(',');
}

async function main(): Promise<void> {
  const repository = resolve(import.meta.dir, '../../..');
  const release = resolve(process.argv[2] ?? join(repository, 'browser-alpha/release'));
  const hot = new Uint8Array(await readFile(join(release, 'hot.bin')));
  const hotSha256 = createHash('sha256').update(hot).digest('hex');
  if (hotSha256 !== QUALIFIED_HOT_SHA256) {
    throw new Error(`hot.bin digest ${hotSha256}; expected ${QUALIFIED_HOT_SHA256}`);
  }
  const cases = await corpusCases(repository);
  cases.push(
    { name: 'utf16:astral', request: { text: '😀', limit: 1, normalizePunctuation: false } },
    {
      name: 'utf16:lone-high',
      request: { text: String.fromCharCode(0xd83d), limit: 1, normalizePunctuation: false }
    },
    {
      name: 'utf16:lone-low',
      request: { text: String.fromCharCode(0xde00), limit: 1, normalizePunctuation: false }
    }
  );
  if (cases.length !== 1_239) throw new Error(`C corpus has ${cases.length} total requests`);
  const analyzer = await frozenAnalyzer(hot);
  const metadata = {
    format: 'ichiran-c-parity-v1',
    operations: cases.length,
    cleanOperations: 1_236,
    utf16: 3,
    suites: { segmentation: 534, cli: 252, hard: 149, counters: 200, entities: 54, probes: 47 },
    oracle: 'frozen TypeScript packages/core/src/analyzer.ts',
    sourceRevision: sourceRevision(repository),
    pack: {
      tag: 'portable-core-260118-baseline',
      hotSha256
    }
  };
  process.stdout.write(`#${JSON.stringify(metadata)}\n`);
  for (let index = 0; index < cases.length; index++) {
    const current = cases[index]!;
    const options = JSON.stringify({
      limit: current.request.limit,
      entities: current.request.entities ?? [],
      normalizePunctuation: current.request.normalizePunctuation
    });
    const result = await analyzer.analyze(current.request);
    const expected = JSON.stringify({ ...result, computeMs: 0 });
    process.stdout.write(`${current.name}\t${utf16Hex(current.request.text)}\t${options}\t${expected}\n`);
    if ((index + 1) % 100 === 0 || index + 1 === cases.length) {
      console.error(`C parity oracle: ${index + 1}/${cases.length}`);
    }
  }
}

await main();
