#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { spawn } from 'node:child_process';
import { readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

import {
  ANALYZER_ANNOTATIONS_SECTION_ID,
  AnalyzerAnnotationNotLoadedError,
  AnalyzerAnnotationsReader,
  analyzerAnnotationsMemorySource
} from '../src/analyzer-annotations.js';
import {
  PortableAnalyzer,
  type PortableAnalysisResult
} from '../src/analyzer.js';
import { ANALYZER_SUPPORT_SECTION_ID, openAnalyzerSupport } from '../src/analyzer-support.js';
import { MORPHOLOGY_SECTION_ID, openMorphology } from '../src/morphology.js';
import { openPack } from '../src/pack.js';
import { ROOT_PAYLOAD_SECTION_ID, openRootPayload } from '../src/root-payload.js';
import { SURFACE_INDEX_SECTION_ID, openSurfaceIndex } from '../src/surface-index.js';
import {
  firstCanonicalDifference,
  projectPortableCleanAnalysis,
  type CanonicalDifference,
  type CleanAnalysisResult
} from './parity-canonical.js';
import {
  loadAnalyzerParityCorpus,
  type AnalyzerFixtureRequest
} from './parity-corpus.js';

interface BatchEntity {
  readonly start: number;
  readonly end: number;
  readonly boost?: number;
}

interface BatchRequest {
  readonly text: string;
  readonly limit: number;
  readonly entities?: readonly BatchEntity[];
  readonly normalizePunctuation: boolean;
}

interface PackedCase {
  readonly suite: 'segmentation' | 'cli' | 'hard' | 'counters' | 'entities' | 'probes';
  readonly index: number;
  readonly request: BatchRequest;
}

interface FallbackCase {
  readonly request: AnalyzerFixtureRequest;
  readonly entities?: readonly BatchEntity[];
  readonly clean: unknown;
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function detailedRequest(
  request: AnalyzerFixtureRequest,
  entities?: readonly BatchEntity[]
): BatchRequest {
  return {
    text: request.text,
    limit: request.limit,
    entities,
    normalizePunctuation: request.normalizePunctuation ?? true
  };
}

function packedCases(corpus: Awaited<ReturnType<typeof loadAnalyzerParityCorpus>>): PackedCase[] {
  const result: PackedCase[] = [];
  const append = (
    suite: PackedCase['suite'],
    requests: readonly BatchRequest[]
  ): void => {
    requests.forEach((request, index) => result.push({ suite, index, request }));
  };
  append('segmentation', corpus.segmentation.map(value => ({
    text: value.input,
    limit: 1,
    normalizePunctuation: false
  })));
  append('cli', corpus.cli.map(value => detailedRequest(value)));
  append('hard', corpus.hard.map(value => detailedRequest(value)));
  append('counters', corpus.counters.map(value => detailedRequest(value)));
  append('entities', corpus.entities.map(value => detailedRequest(
    { text: value.text, limit: 1 },
    value.entities
  )));
  append('probes', corpus.probes.map(value => detailedRequest(value.request)));
  const expected = {
    segmentation: 534,
    cli: 252,
    hard: 149,
    counters: 200,
    entities: 54,
    probes: 47
  } as const;
  for (const [suite, count] of Object.entries(expected)) {
    const actual = result.filter(value => value.suite === suite).length;
    if (actual !== count) throw new Error(`${suite} has ${actual} requests; expected ${count}`);
  }
  if (result.length !== 1_236) {
    throw new Error(`packed analyzer corpus has ${result.length} requests; expected 1236`);
  }
  return result;
}

async function rustBatch(
  repository: string,
  hotPath: string,
  requests: readonly BatchRequest[]
): Promise<PortableAnalysisResult[]> {
  const child = spawn('cargo', [
    'run', '--quiet',
    '--manifest-path', join(repository, 'packages/rust-kernel/Cargo.toml'),
    '--bin', 'analyzer_batch', '--', hotPath
  ], {
    cwd: repository,
    stdio: ['pipe', 'pipe', 'pipe']
  });
  const stdout: Buffer[] = [];
  const stderr: Buffer[] = [];
  child.stdout.on('data', value => stdout.push(value as Buffer));
  child.stderr.on('data', value => stderr.push(value as Buffer));
  child.stdin.end(JSON.stringify(requests));
  const status = await new Promise<number | null>((resolveStatus, reject) => {
    child.once('error', reject);
    child.once('close', resolveStatus);
  });
  if (status !== 0) {
    throw new Error(
      `native analyzer batch exited ${String(status)}: ${Buffer.concat(stderr).toString('utf8')}`
    );
  }
  const values = JSON.parse(Buffer.concat(stdout).toString('utf8')) as PortableAnalysisResult[];
  if (!Array.isArray(values) || values.length !== requests.length) {
    throw new Error(`native analyzer batch returned ${values.length} results; expected ${requests.length}`);
  }
  return values;
}

async function frozenAnalyzer(hot: Uint8Array): Promise<{
  analyze(request: BatchRequest): Promise<PortableAnalysisResult>;
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
    async analyze(request): Promise<PortableAnalysisResult> {
      const loaded = new Set<string>();
      try {
        for (;;) {
          try {
            return analyzer.analyze(request.text, {
              limit: request.limit,
              entities: request.entities,
              normalizePunctuation: request.normalizePunctuation
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

function rawDifference(expected: unknown, actual: unknown, path = '$'): CanonicalDifference | null {
  if (Object.is(expected, actual)) return null;
  if (Array.isArray(expected) || Array.isArray(actual)) {
    if (!Array.isArray(expected) || !Array.isArray(actual)) {
      return { path, kind: 'type', expected, actual };
    }
    if (expected.length !== actual.length) {
      return { path, kind: 'length', expected: expected.length, actual: actual.length };
    }
    for (let index = 0; index < expected.length; index++) {
      const difference = rawDifference(expected[index], actual[index], `${path}[${index}]`);
      if (difference) return difference;
    }
    return null;
  }
  const expectedObject = typeof expected === 'object' && expected !== null;
  const actualObject = typeof actual === 'object' && actual !== null;
  if (expectedObject || actualObject) {
    if (!expectedObject || !actualObject) return { path, kind: 'type', expected, actual };
    const left = expected as Record<string, unknown>;
    const right = actual as Record<string, unknown>;
    const keys = [...new Set([...Object.keys(left), ...Object.keys(right)])].sort();
    for (const key of keys) {
      if (!(key in left) || !(key in right)) {
        return { path: `${path}.${key}`, kind: 'missing', expected: left[key], actual: right[key] };
      }
      const difference = rawDifference(left[key], right[key], `${path}.${key}`);
      if (difference) return difference;
    }
    return null;
  }
  return { path, kind: typeof expected === typeof actual ? 'value' : 'type', expected, actual };
}

function fallbackCases(value: unknown): Record<'counters' | 'entities' | 'probes', FallbackCase[]> {
  const fixture = value as {
    readonly formatVersion?: number;
    readonly identityPolicy?: string;
    readonly counts?: { readonly total?: number };
    readonly suites?: Record<string, FallbackCase[]>;
  };
  if (
    fixture.formatVersion !== 1
    || fixture.identityPolicy !== 'terminal-root-v1'
    || fixture.counts?.total !== 301
    || !fixture.suites
  ) throw new Error('fallback fixture identity or accounting is invalid');
  const result = {
    counters: fixture.suites.counters,
    entities: fixture.suites.entities,
    probes: fixture.suites.probes
  };
  for (const [suite, count] of [['counters', 200], ['entities', 54], ['probes', 47]] as const) {
    if (!Array.isArray(result[suite]) || result[suite].length !== count) {
      throw new Error(`fallback ${suite} has ${result[suite]?.length ?? 'invalid'} cases; expected ${count}`);
    }
  }
  return result;
}

function rootSequences(result: CleanAnalysisResult): Array<number | null> {
  return result.paths[0]?.tokens[0]?.alternatives.map(value => value.root?.seq ?? null) ?? [];
}

async function main(): Promise<void> {
  const repository = resolve(import.meta.dir, '../../..');
  const release = resolve(process.argv[2] ?? join(repository, 'browser-alpha/release'));
  const hotPath = join(release, 'hot.bin');
  const fixturePath = join(repository, 'packages/rust-kernel/tests/fixtures/m3-fallback.json');
  const [hot, fixtureBytes, corpus] = await Promise.all([
    readFile(hotPath),
    readFile(fixturePath),
    loadAnalyzerParityCorpus(repository)
  ]);
  const cases = packedCases(corpus);
  const rustResults = await rustBatch(repository, hotPath, cases.map(value => value.request));
  const typescript = await frozenAnalyzer(new Uint8Array(hot));
  const cleanRust = rustResults.map(projectPortableCleanAnalysis);
  const cleanTypescript: CleanAnalysisResult[] = [];
  for (let index = 0; index < cases.length; index++) {
    cleanTypescript.push(projectPortableCleanAnalysis(await typescript.analyze(cases[index]!.request)));
    if ((index + 1) % 100 === 0 || index + 1 === cases.length) {
      console.error(`raw differential: ${index + 1}/${cases.length}`);
    }
  }

  const rawSuites: Record<string, { operations: number; exact: number; divergent: number }> = {};
  const rawDifferences: unknown[] = [];
  for (let index = 0; index < cases.length; index++) {
    const current = cases[index]!;
    const stats = rawSuites[current.suite] ??= { operations: 0, exact: 0, divergent: 0 };
    stats.operations++;
    const difference = rawDifference(cleanTypescript[index], cleanRust[index]);
    if (!difference) stats.exact++;
    else {
      stats.divergent++;
      if (rawDifferences.length < 10) {
        rawDifferences.push({
          suite: current.suite,
          index: current.index,
          request: current.request,
          difference
        });
      }
    }
  }

  const packedOffsets = new Map<string, number>();
  cases.forEach((value, index) => packedOffsets.set(`${value.suite}:${value.index}`, index));
  const fallback = fallbackCases(JSON.parse(fixtureBytes.toString('utf8')) as unknown);
  const fallbackSuites: Record<string, { operations: number; exact: number; divergent: number }> = {};
  const fallbackDifferences: unknown[] = [];
  for (const suite of ['counters', 'entities', 'probes'] as const) {
    const stats = fallbackSuites[suite] = { operations: 0, exact: 0, divergent: 0 };
    for (let index = 0; index < fallback[suite].length; index++) {
      const expected = fallback[suite][index]!;
      const offset = packedOffsets.get(`${suite}:${index}`);
      if (offset === undefined) throw new Error(`packed corpus is missing fallback ${suite}[${index}]`);
      const packed = cases[offset]!.request;
      const wanted = detailedRequest(expected.request, expected.entities);
      if (rawDifference(wanted, packed)) {
        throw new Error(`fallback ${suite}[${index}] request disagrees with packed corpus`);
      }
      stats.operations++;
      const difference = firstCanonicalDifference(expected.clean, cleanRust[offset]);
      if (!difference) stats.exact++;
      else {
        stats.divergent++;
        if (fallbackDifferences.length < 10) {
          fallbackDifferences.push({ suite, index, request: wanted, difference });
        }
      }
    }
  }

  const tieOffset = cases.findIndex(value => value.suite === 'probes' && value.request.text === 'ﾊｼ');
  if (tieOffset < 0) throw new Error('packed corpus has no halfwidth-katakana tie witness');
  const tieTypescript = rootSequences(cleanTypescript[tieOffset]!);
  const tieRust = rootSequences(cleanRust[tieOffset]!);
  const rawExact = Object.values(rawSuites).reduce((sum, value) => sum + value.exact, 0);
  const rawDivergent = cases.length - rawExact;
  const fallbackExact = Object.values(fallbackSuites).reduce((sum, value) => sum + value.exact, 0);
  const report = {
    formatVersion: 1,
    pack: { path: hotPath, sha256: sha256(hot) },
    corpus: { operations: cases.length, suites: rawSuites },
    rawPresentationFree: {
      policy: 'Exact object values and exact array order; no equal-score canonicalization.',
      operations: cases.length,
      exact: rawExact,
      divergent: rawDivergent,
      firstDifferences: rawDifferences,
      halfwidthKatakanaTie: {
        request: 'ﾊｼ',
        exact: rawDifference(tieTypescript, tieRust) === null,
        typescriptRootSequences: tieTypescript,
        rustRootSequences: tieRust
      }
    },
    fallback: {
      fixture: { path: fixturePath, sha256: sha256(fixtureBytes) },
      policy: 'Repository canonical equal-score path/alternative ordering only.',
      operations: 301,
      exact: fallbackExact,
      divergent: 301 - fallbackExact,
      suites: fallbackSuites,
      firstDifferences: fallbackDifferences
    },
    allowlist: { entries: 0 }
  };
  console.log(JSON.stringify(report, null, 2));
  if (rawDivergent !== 0 || fallbackExact !== 301) process.exitCode = 1;
}

await main();
