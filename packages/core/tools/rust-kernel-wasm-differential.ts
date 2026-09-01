#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { createReadStream } from 'node:fs';
import { readFile, stat } from 'node:fs/promises';
import { join, resolve } from 'node:path';

import {
  IchiranRuntime,
  type PortableAnalyzeOptions
} from '../src/index.js';

import { TypeScriptOracleRuntime } from '../src/runtime-typescript.js';
import { compareDetailedAuthority } from './oracle-authority.js';
import {
  firstCanonicalDifference,
  projectPortableCleanAnalysis,
  type CanonicalDifference,
  type CleanAnalysisResult
} from './parity-canonical.js';
import {
  fixtureKey,
  loadAnalyzerParityCorpus,
  type AnalyzerFixtureRequest,
  type AnalyzerParityCorpus
} from './parity-corpus.js';

const QUALIFIED = {
  hot: {
    bytes: 24_857_288,
    sha256: '61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0'
  },
  details: {
    bytes: 13_555_874,
    sha256: '0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151'
  },
  wasm: {
    bytes: 1_119_198,
    sha256: 'd8b35fbd8f3d62ef63724f4df833deb8c40a76053d1b3ce84459a81ff04d55eb'
  }
} as const;
const FALLBACK_SHA256 = 'dbc13ead615b8d70d2f3ecf38aeb7042361459856700a86844c5fe0db6706843';

type SuiteName = 'segmentation' | 'cli' | 'hard' | 'counters' | 'entities' | 'probes';
type DetailedSuiteName = Exclude<SuiteName, 'segmentation'>;
type Authority = 'current-lisp' | 'frozen-postgres-reference';

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

interface RawCase {
  readonly suite: SuiteName;
  readonly index: number;
  readonly request: BatchRequest;
}

interface DetailedCase {
  readonly suite: DetailedSuiteName;
  readonly index: number;
  readonly rawOffset: number;
  readonly request: BatchRequest;
  readonly authority: Authority;
  readonly expected: unknown;
  readonly fallbackClean?: unknown;
}

interface FallbackCase {
  readonly request: AnalyzerFixtureRequest;
  readonly entities?: readonly BatchEntity[];
  readonly clean: unknown;
  readonly detailed: unknown;
}

interface ExactStats {
  operations: number;
  exact: number;
  divergent: number;
}

function digest(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function fileDigest(path: string): Promise<string> {
  const hash = createHash('sha256');
  for await (const chunk of createReadStream(path)) hash.update(chunk);
  return hash.digest('hex');
}

function verifyArtifact(
  label: keyof typeof QUALIFIED,
  bytes: number,
  sha256: string
): void {
  const expected = QUALIFIED[label];
  if (bytes !== expected.bytes || sha256 !== expected.sha256) {
    throw new Error(
      `${label} is ${bytes} bytes with SHA-256 ${sha256}; expected ${expected.bytes} and ${expected.sha256}`
    );
  }
}

function batchRequest(
  request: AnalyzerFixtureRequest,
  entities?: readonly BatchEntity[]
): BatchRequest {
  return {
    text: request.text,
    limit: request.limit,
    ...(entities ? { entities } : {}),
    normalizePunctuation: request.normalizePunctuation ?? true
  };
}

function options(request: BatchRequest): PortableAnalyzeOptions {
  return {
    limit: request.limit,
    ...(request.entities ? { entities: request.entities } : {}),
    normalizePunctuation: request.normalizePunctuation
  };
}

function sameRequest(left: BatchRequest, right: BatchRequest): boolean {
  const leftEntities = left.entities ?? [];
  const rightEntities = right.entities ?? [];
  return left.text === right.text
    && left.limit === right.limit
    && left.normalizePunctuation === right.normalizePunctuation
    && leftEntities.length === rightEntities.length
    && leftEntities.every((entity, index) => {
      const other = rightEntities[index]!;
      return entity.start === other.start
        && entity.end === other.end
        && entity.boost === other.boost;
    });
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

function rawCases(corpus: AnalyzerParityCorpus): RawCase[] {
  const result: RawCase[] = [];
  const append = (suite: SuiteName, requests: readonly BatchRequest[]): void => {
    requests.forEach((request, index) => result.push({ suite, index, request }));
  };
  append('segmentation', corpus.segmentation.map(value => ({
    text: value.input,
    limit: 1,
    normalizePunctuation: false
  })));
  append('cli', corpus.cli.map(value => batchRequest(value)));
  append('hard', corpus.hard.map(value => batchRequest(value)));
  append('counters', corpus.counters.map(value => batchRequest(value)));
  append('entities', corpus.entities.map(value => batchRequest(
    { text: value.text, limit: 1 },
    value.entities
  )));
  append('probes', corpus.probes.map(value => batchRequest(value.request)));
  const counts = { segmentation: 534, cli: 252, hard: 149, counters: 200, entities: 54, probes: 47 };
  for (const [suite, count] of Object.entries(counts)) {
    const actual = result.filter(value => value.suite === suite).length;
    if (actual !== count) throw new Error(`${suite} has ${actual} requests; expected ${count}`);
  }
  if (result.length !== 1_236) {
    throw new Error(`raw WASM corpus has ${result.length} requests; expected 1236`);
  }
  return result;
}

function fallbackSuites(value: unknown): Record<'counters' | 'entities' | 'probes', FallbackCase[]> {
  const fixture = value as {
    readonly formatVersion?: number;
    readonly identityPolicy?: string;
    readonly counts?: Record<string, number>;
    readonly suites?: Record<string, FallbackCase[]>;
  };
  if (
    fixture.formatVersion !== 1
    || fixture.identityPolicy !== 'terminal-root-v1'
    || fixture.counts?.counters !== 200
    || fixture.counts?.entities !== 54
    || fixture.counts?.probes !== 47
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

function detailedCases(
  corpus: AnalyzerParityCorpus,
  raw: readonly RawCase[],
  fallback: Record<'counters' | 'entities' | 'probes', FallbackCase[]>
): DetailedCase[] {
  const offsets = new Map<string, number>();
  raw.forEach((value, index) => offsets.set(`${value.suite}:${value.index}`, index));
  const result: DetailedCase[] = [];
  for (const suite of ['cli', 'hard'] as const) {
    const requests = corpus[suite];
    const outputs = suite === 'cli' ? corpus.currentLispCli : corpus.currentLispHard;
    requests.forEach((request, index) => {
      const serialized = outputs[fixtureKey(request)];
      const rawOffset = offsets.get(`${suite}:${index}`);
      if (serialized === undefined || rawOffset === undefined) {
        throw new Error(`${suite}[${index}] is missing its current-Lisp authority or raw request`);
      }
      result.push({
        suite,
        index,
        rawOffset,
        request: raw[rawOffset]!.request,
        authority: 'current-lisp',
        expected: JSON.parse(serialized) as unknown
      });
    });
  }
  for (const suite of ['counters', 'entities', 'probes'] as const) {
    fallback[suite].forEach((fixture, index) => {
      const rawOffset = offsets.get(`${suite}:${index}`);
      if (rawOffset === undefined) throw new Error(`${suite}[${index}] has no raw request`);
      const request = raw[rawOffset]!.request;
      if (!sameRequest(batchRequest(fixture.request, fixture.entities), request)) {
        throw new Error(`fallback ${suite}[${index}] request disagrees with the packed corpus`);
      }
      result.push({
        suite,
        index,
        rawOffset,
        request,
        authority: 'frozen-postgres-reference',
        expected: fixture.detailed,
        fallbackClean: fixture.clean
      });
    });
  }
  if (result.length !== 702) {
    throw new Error(`detailed WASM corpus has ${result.length} requests; expected 702`);
  }
  return result;
}

function source(file: ReturnType<typeof Bun.file>): {
  readonly byteLength: number;
  readonly reads: Array<{ readonly offset: number; readonly byteLength: number }>;
  readonly read: (offset: number, byteLength: number) => Promise<Uint8Array>;
} {
  const reads: Array<{ offset: number; byteLength: number }> = [];
  return {
    byteLength: file.size,
    reads,
    async read(offset, byteLength): Promise<Uint8Array> {
      reads.push({ offset, byteLength });
      return new Uint8Array(await file.slice(offset, offset + byteLength).arrayBuffer());
    }
  };
}

function stats(names: readonly string[]): Record<string, ExactStats> {
  return Object.fromEntries(names.map(name => [name, { operations: 0, exact: 0, divergent: 0 }]));
}

function progress(label: string, completed: number, total: number): void {
  if (completed % 50 === 0 || completed === total) {
    console.error(`${label}: ${completed}/${total}`);
  }
}

function rootSequences(result: CleanAnalysisResult): Array<number | null> {
  return result.paths[0]?.tokens[0]?.alternatives.map(value => value.root?.seq ?? null) ?? [];
}

async function main(): Promise<void> {
  const repository = resolve(import.meta.dir, '../../..');
  const release = resolve(process.argv[2] ?? join(repository, 'browser-alpha/release'));
  const wasmPath = resolve(
    process.argv[3] ?? join(repository, 'packages/core/src/rust-kernel/generated/ichiran_kernel_bg.wasm')
  );
  const hotPath = join(release, 'hot.bin');
  const detailsPath = join(release, 'details.bin');
  const fallbackPath = join(repository, 'packages/rust-kernel/tests/fixtures/m3-fallback.json');
  const [hotBytes, wasmBytes, detailsStat, detailsSha256, fixtureBytes, corpus] = await Promise.all([
    readFile(hotPath),
    readFile(wasmPath),
    stat(detailsPath),
    fileDigest(detailsPath),
    readFile(fallbackPath),
    loadAnalyzerParityCorpus(repository)
  ]);
  verifyArtifact('hot', hotBytes.byteLength, digest(hotBytes));
  verifyArtifact('details', detailsStat.size, detailsSha256);
  verifyArtifact('wasm', wasmBytes.byteLength, digest(wasmBytes));
  const fallbackSha256 = digest(fixtureBytes);
  if (fallbackSha256 !== FALLBACK_SHA256) {
    throw new Error(`fallback fixture SHA-256 ${fallbackSha256}; expected ${FALLBACK_SHA256}`);
  }

  const raw = rawCases(corpus);
  const fallback = fallbackSuites(JSON.parse(fixtureBytes.toString('utf8')) as unknown);
  const detailed = detailedCases(corpus, raw, fallback);
  const detailFile = Bun.file(detailsPath);
  const wasmDetails = source(detailFile);
  const oracleDetails = source(detailFile);
  const decodeGzip = async (compressed: Uint8Array, expectedByteLength: number): Promise<Uint8Array> => {
    const decoded = new Uint8Array(Bun.gunzipSync(compressed));
    if (decoded.byteLength !== expectedByteLength) {
      throw new Error(`gzip decoded ${decoded.byteLength} bytes; expected ${expectedByteLength}`);
    }
    return decoded;
  };
  const oracle = await TypeScriptOracleRuntime.open({
    hot: new Uint8Array(hotBytes),
    details: oracleDetails,
    decodeGzip
  });
  const wasm = await IchiranRuntime.open({
    hot: new Uint8Array(hotBytes),
    details: wasmDetails,
    decodeGzip,
    wasm: new Uint8Array(wasmBytes)
  });

  const rawStats = stats(['segmentation', 'cli', 'hard', 'counters', 'entities', 'probes']);
  const rawDifferences: unknown[] = [];
  const wasmClean: CleanAnalysisResult[] = [];
  const oracleClean: CleanAnalysisResult[] = [];
  const fallbackStats = stats(['counters', 'entities', 'probes']);
  const fallbackDifferences: unknown[] = [];
  const detailedStats = stats(['cli', 'hard', 'counters', 'entities', 'probes']);
  const authorityStats = stats(['current-lisp', 'frozen-postgres-reference']);
  const detailedDifferences: unknown[] = [];

  try {
    for (let index = 0; index < raw.length; index++) {
      const fixture = raw[index]!;
      const operationOptions = options(fixture.request);
      const [actual, expected] = await Promise.all([
        wasm.analyze(fixture.request.text, operationOptions),
        oracle.analyze(fixture.request.text, operationOptions)
      ]);
      const actualClean = projectPortableCleanAnalysis(actual);
      const expectedClean = projectPortableCleanAnalysis(expected);
      wasmClean.push(actualClean);
      oracleClean.push(expectedClean);
      const difference = rawDifference(expectedClean, actualClean);
      const suite = rawStats[fixture.suite]!;
      suite.operations++;
      if (difference) {
        suite.divergent++;
        rawDifferences.push({
          suite: fixture.suite,
          index: fixture.index,
          request: fixture.request,
          difference
        });
      } else suite.exact++;
      progress('WASM raw differential', index + 1, raw.length);
    }

    for (const fixture of detailed) {
      if (fixture.fallbackClean === undefined) continue;
      const difference = firstCanonicalDifference(
        fixture.fallbackClean,
        wasmClean[fixture.rawOffset]
      );
      const suite = fallbackStats[fixture.suite]!;
      suite.operations++;
      if (difference) {
        suite.divergent++;
        fallbackDifferences.push({
          suite: fixture.suite,
          index: fixture.index,
          request: fixture.request,
          difference
        });
      } else suite.exact++;
    }

    for (let index = 0; index < detailed.length; index++) {
      const fixture = detailed[index]!;
      const actual = await wasm.legacy(fixture.request.text, options(fixture.request));
      const comparison = compareDetailedAuthority(
        fixture.authority === 'current-lisp' ? fixture.expected : null,
        fixture.authority === 'frozen-postgres-reference' ? fixture.expected : null,
        actual
      );
      if (comparison.source !== fixture.authority) {
        throw new Error(`${fixture.suite}[${fixture.index}] selected the wrong authority`);
      }
      for (const suite of [detailedStats[fixture.suite]!, authorityStats[fixture.authority]!]) {
        suite.operations++;
        if (comparison.detailedDifference) suite.divergent++;
        else suite.exact++;
      }
      if (comparison.detailedDifference) {
        detailedDifferences.push({
          suite: fixture.suite,
          index: fixture.index,
          request: fixture.request,
          authority: fixture.authority,
          pathDifference: comparison.pathDifference,
          detailedDifference: comparison.detailedDifference
        });
      }
      progress('WASM detailed differential', index + 1, detailed.length);
    }
  } finally {
    wasm.dispose();
  }

  const rawExact = Object.values(rawStats).reduce((sum, value) => sum + value.exact, 0);
  const fallbackExact = Object.values(fallbackStats).reduce((sum, value) => sum + value.exact, 0);
  const detailedExact = Object.values(detailedStats).reduce((sum, value) => sum + value.exact, 0);
  const tieOffset = raw.findIndex(value => value.suite === 'probes' && value.request.text === 'ﾊｼ');
  if (tieOffset < 0) throw new Error('raw corpus has no halfwidth-katakana tie witness');
  const wasmTie = rootSequences(wasmClean[tieOffset]!);
  const oracleTie = rootSequences(oracleClean[tieOffset]!);
  const report = {
    formatVersion: 1,
    artifacts: {
      hot: { path: hotPath, ...QUALIFIED.hot },
      details: { path: detailsPath, ...QUALIFIED.details },
      wasm: { path: wasmPath, ...QUALIFIED.wasm },
      fallback: { path: fallbackPath, sha256: fallbackSha256 }
    },
    rawPresentationFree: {
      policy: 'Exact object values and exact array order against the frozen TypeScript oracle; no tie canonicalization.',
      operations: raw.length,
      exact: rawExact,
      divergent: raw.length - rawExact,
      suites: rawStats,
      differences: rawDifferences,
      halfwidthKatakanaTie: {
        request: 'ﾊｼ',
        exact: rawDifference(oracleTie, wasmTie) === null,
        oracleRootSequences: oracleTie,
        wasmRootSequences: wasmTie
      }
    },
    fallbackClean: {
      policy: 'Provenance-bound frozen fallback with repository canonical equal-score ordering only.',
      operations: 301,
      exact: fallbackExact,
      divergent: 301 - fallbackExact,
      suites: fallbackStats,
      differences: fallbackDifferences
    },
    detailedRetainedLegacy: {
      policy: 'Current-Lisp snapshots are authoritative where present; provenance-bound frozen PostgreSQL output is the fallback.',
      operations: detailed.length,
      exact: detailedExact,
      divergent: detailed.length - detailedExact,
      suites: detailedStats,
      authority: authorityStats,
      differences: detailedDifferences
    },
    detailRandomAccess: {
      reads: wasmDetails.reads.length,
      bytes: wasmDetails.reads.reduce((sum, value) => sum + value.byteLength, 0),
      maximumReadBytes: Math.max(...wasmDetails.reads.map(value => value.byteLength)),
      wholeFileRead: wasmDetails.reads.some(value => value.byteLength === detailFile.size)
    },
    allowlist: { entries: 0 }
  };
  console.log(JSON.stringify(report, null, 2));
  if (rawExact !== 1_236 || fallbackExact !== 301 || detailedExact !== 702) {
    process.exitCode = 1;
  }
}

await main();
