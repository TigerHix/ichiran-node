#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { createReadStream } from 'node:fs';
import { readFile, stat } from 'node:fs/promises';
import { join, resolve } from 'node:path';

import {
  IchiranRuntime,
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseAsset,
  type PortableAnalysisResult,
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
    bytes: 1_119_555,
    sha256: 'f4d17d3a406c1c8269acfc54cd4b08fcaaee795f1d273f8af93be6b25331fe5d'
  }
} as const;
const FALLBACK_SHA256 = 'dbc13ead615b8d70d2f3ecf38aeb7042361459856700a86844c5fe0db6706843';

type SuiteName = 'segmentation' | 'cli' | 'hard' | 'counters' | 'entities' | 'probes';
type DetailedSuiteName = Exclude<SuiteName, 'segmentation'>;
type Authority = 'current-lisp' | 'frozen-postgres-reference';
type DifferentialMode = 'immutable-baseline' | 'same-pack';

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

interface DetailedRequestCase {
  readonly suite: DetailedSuiteName;
  readonly index: number;
  readonly rawOffset: number;
  readonly request: BatchRequest;
}

interface DetailedCase extends DetailedRequestCase {
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

interface RequestFile {
  readonly romanization: readonly string[];
  readonly fullJson: readonly AnalyzerFixtureRequest[];
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

function verifyInstalledAsset(
  label: 'hot' | 'details',
  asset: AnalyzerReleaseAsset,
  bytes: number,
  sha256: string
): void {
  if (bytes !== asset.installedBytes || sha256 !== asset.installedSha256) {
    throw new Error(
      `${label} is ${bytes} bytes with SHA-256 ${sha256}; manifest identifies `
      + `${asset.installedBytes} and ${asset.installedSha256}`
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

/** Same-pack qualification ignores elapsed time and compares every other public DTO field. */
export function samePackAnalysisDifference(
  expected: PortableAnalysisResult,
  actual: PortableAnalysisResult
): CanonicalDifference | null {
  return rawDifference(
    { ...expected, computeMs: 0 },
    { ...actual, computeMs: 0 }
  );
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

async function loadSamePackCorpus(
  repository: string,
  fixtureBytes: Uint8Array
): Promise<AnalyzerParityCorpus> {
  const [segmentation, cli, hard] = await Promise.all([
    readFile(join(repository, 'packages/reference-postgres/tests/data/segmentation.json'))
      .then(bytes => JSON.parse(bytes.toString('utf8')) as AnalyzerParityCorpus['segmentation']),
    readFile(join(repository, 'packages/cli/tests/data/cli.json'))
      .then(bytes => JSON.parse(bytes.toString('utf8')) as RequestFile),
    readFile(join(repository, 'packages/cli/tests/data/hard-cli.json'))
      .then(bytes => JSON.parse(bytes.toString('utf8')) as RequestFile)
  ]);
  const fallback = fallbackSuites(JSON.parse(new TextDecoder().decode(fixtureBytes)) as unknown);
  return {
    segmentation,
    romanization: cli.romanization,
    cli: cli.fullJson,
    hard: hard.fullJson,
    counters: fallback.counters.map(value => value.request),
    entities: fallback.entities.map((value, index) => ({
      title: `same-pack-${index}`,
      text: value.request.text,
      entities: value.entities ?? []
    })),
    probes: fallback.probes.map((value, index) => ({
      category: 'top-n',
      name: `same-pack-${index}`,
      request: value.request
    })),
    currentLispCli: {},
    currentLispHard: {},
    currentLispRomanization: {}
  };
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

function samePackDetailedCases(raw: readonly RawCase[]): DetailedRequestCase[] {
  const result = raw.flatMap((value, rawOffset) => value.suite === 'segmentation'
    ? []
    : [{
        suite: value.suite,
        index: value.index,
        rawOffset,
        request: value.request
      }]);
  if (result.length !== 702) {
    throw new Error(`same-pack detailed corpus has ${result.length} requests; expected 702`);
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
  const arguments_ = process.argv.slice(2);
  let mode: DifferentialMode = 'immutable-baseline';
  if (arguments_[0] === '--same-pack') {
    arguments_.shift();
    mode = 'same-pack';
  }
  if (arguments_.length > 2) {
    throw new Error(
      'Usage: rust-kernel-wasm-differential.ts [--same-pack] [release-directory] [wasm-file]'
    );
  }
  const release = resolve(arguments_[0] ?? join(repository, 'browser-alpha/release'));
  const wasmPath = resolve(
    arguments_[1] ?? join(repository, 'packages/core/src/rust-kernel/generated/ichiran_kernel_bg.wasm')
  );
  const hotPath = join(release, 'hot.bin');
  const detailsPath = join(release, 'details.bin');
  const manifestPath = join(release, 'manifest.json');
  const fallbackPath = join(repository, 'packages/rust-kernel/tests/fixtures/m3-fallback.json');
  const [
    hotBytes,
    wasmBytes,
    detailsStat,
    detailsSha256,
    fixtureBytes,
    manifestBytes
  ] = await Promise.all([
    readFile(hotPath),
    readFile(wasmPath),
    stat(detailsPath),
    fileDigest(detailsPath),
    readFile(fallbackPath),
    mode === 'same-pack' ? readFile(manifestPath) : Promise.resolve(null)
  ]);
  const corpus = mode === 'immutable-baseline'
    ? await loadAnalyzerParityCorpus(repository)
    : await loadSamePackCorpus(repository, fixtureBytes);
  const hotSha256 = digest(hotBytes);
  const wasmSha256 = digest(wasmBytes);
  if (mode === 'immutable-baseline') {
    verifyArtifact('hot', hotBytes.byteLength, hotSha256);
    verifyArtifact('details', detailsStat.size, detailsSha256);
    verifyArtifact('wasm', wasmBytes.byteLength, wasmSha256);
  } else {
    if (manifestBytes === null) throw new Error('same-pack mode requires manifest.json');
    const manifest = parseAnalyzerReleaseManifest(
      JSON.parse(manifestBytes.toString('utf8')) as unknown,
      value => createHash('sha256').update(value).digest('hex')
    );
    if (manifest.formatVersion !== 1) {
      throw new Error(`same-pack mode requires pack format v1; found ${manifest.formatVersion}`);
    }
    verifyInstalledAsset('hot', manifest.hot, hotBytes.byteLength, hotSha256);
    verifyInstalledAsset('details', manifest.details, detailsStat.size, detailsSha256);
  }
  const fallbackSha256 = digest(fixtureBytes);
  if (mode === 'immutable-baseline' && fallbackSha256 !== FALLBACK_SHA256) {
    throw new Error(`fallback fixture SHA-256 ${fallbackSha256}; expected ${FALLBACK_SHA256}`);
  }

  const raw = rawCases(corpus);
  const fallback = mode === 'immutable-baseline'
    ? fallbackSuites(JSON.parse(fixtureBytes.toString('utf8')) as unknown)
    : null;
  const detailed = fallback === null ? null : detailedCases(corpus, raw, fallback);
  const samePackDetailed = mode === 'same-pack' ? samePackDetailedCases(raw) : [];
  const detailFile = Bun.file(detailsPath);
  const wasmDetails = source(detailFile);
  const oracleDetails = source(detailFile);
  const decodeGzip = async (compressed: Uint8Array, expectedByteLength: number): Promise<Uint8Array> => {
    const decoded = new Uint8Array(Bun.gunzipSync(Uint8Array.from(compressed)));
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
    wasm: new Uint8Array(wasmBytes)
  });
  const wasmOpenDetailReads = [...wasmDetails.reads];

  const rawStats = stats(['segmentation', 'cli', 'hard', 'counters', 'entities', 'probes']);
  const rawDifferences: unknown[] = [];
  const wasmClean: CleanAnalysisResult[] = [];
  const oracleClean: CleanAnalysisResult[] = [];
  const fallbackStats = stats(['counters', 'entities', 'probes']);
  const fallbackDifferences: unknown[] = [];
  const detailedStats = stats(['cli', 'hard', 'counters', 'entities', 'probes']);
  const authorityStats = stats(['current-lisp', 'frozen-postgres-reference']);
  const detailedDifferences: unknown[] = [];
  const romanizationStats: ExactStats = { operations: 0, exact: 0, divergent: 0 };
  const romanizationDifferences: unknown[] = [];

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
      const difference = mode === 'same-pack'
        ? samePackAnalysisDifference(expected, actual)
        : rawDifference(expectedClean, actualClean);
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

    for (let index = 0; index < corpus.romanization.length; index++) {
      const input = corpus.romanization[index]!;
      const [actual, oracleValue] = await Promise.all([
        wasm.romanize(input),
        oracle.romanize(input)
      ]);
      const authority = mode === 'immutable-baseline'
        ? corpus.currentLispRomanization[input]
        : oracleValue;
      if (authority === undefined) {
        throw new Error(`romanization[${index}] has no current-Lisp authority`);
      }
      const oracleDifference = rawDifference(authority, oracleValue);
      const wasmDifference = rawDifference(oracleValue, actual);
      romanizationStats.operations++;
      if (oracleDifference || wasmDifference) {
        romanizationStats.divergent++;
        romanizationDifferences.push({
          index,
          input,
          oracleDifference,
          wasmDifference
        });
      } else romanizationStats.exact++;
    }

    if (detailed !== null) {
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
    } else {
      for (let index = 0; index < samePackDetailed.length; index++) {
        const fixture = samePackDetailed[index]!;
        const [actual, expected] = await Promise.all([
          wasm.legacy(fixture.request.text, options(fixture.request)),
          oracle.legacy(fixture.request.text, options(fixture.request))
        ]);
        const difference = rawDifference(expected, actual);
        const suite = detailedStats[fixture.suite]!;
        suite.operations++;
        if (difference) {
          suite.divergent++;
          detailedDifferences.push({
            suite: fixture.suite,
            index: fixture.index,
            request: fixture.request,
            difference
          });
        } else suite.exact++;
        progress('WASM same-pack detailed differential', index + 1, samePackDetailed.length);
      }
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
    mode,
    artifacts: mode === 'immutable-baseline' ? {
      hot: { path: hotPath, ...QUALIFIED.hot },
      details: { path: detailsPath, ...QUALIFIED.details },
      wasm: { path: wasmPath, ...QUALIFIED.wasm },
      fallback: { path: fallbackPath, sha256: fallbackSha256 }
    } : {
      manifest: { path: manifestPath, sha256: digest(manifestBytes!) },
      hot: { path: hotPath, bytes: hotBytes.byteLength, sha256: hotSha256 },
      details: { path: detailsPath, bytes: detailsStat.size, sha256: detailsSha256 },
      wasm: { path: wasmPath, bytes: wasmBytes.byteLength, sha256: wasmSha256 }
    },
    rawAnalyzer: {
      policy: mode === 'same-pack'
        ? 'Complete public DTO equality against the TypeScript same-pack oracle after normalizing only computeMs; exact array order and candidateId values.'
        : 'Exact presentation-free object values and exact array order against the frozen TypeScript oracle; no tie canonicalization.',
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
    standaloneRomanization: {
      policy: mode === 'immutable-baseline'
        ? 'Rust and the frozen TypeScript oracle must both exactly match the pinned current-Lisp string.'
        : 'Exact string equality between Rust and the frozen TypeScript oracle reading the same format-v1 pack.',
      ...romanizationStats,
      differences: romanizationDifferences
    },
    ...(mode === 'immutable-baseline' ? {
      fallbackClean: {
        policy: 'Provenance-bound frozen fallback with repository canonical equal-score ordering only.',
        operations: 301,
        exact: fallbackExact,
        divergent: 301 - fallbackExact,
        suites: fallbackStats,
        differences: fallbackDifferences
      }
    } : {}),
    detailedRetainedLegacy: {
      policy: mode === 'immutable-baseline'
        ? 'Current-Lisp snapshots are authoritative where present; provenance-bound frozen PostgreSQL output is the fallback.'
        : 'Exact serialized values and array order between Rust and the frozen TypeScript oracle reading the same format-v1 pack.',
      operations: detailed?.length ?? samePackDetailed.length,
      exact: detailedExact,
      divergent: (detailed?.length ?? samePackDetailed.length) - detailedExact,
      suites: detailedStats,
      ...(mode === 'immutable-baseline' ? { authority: authorityStats } : {}),
      differences: detailedDifferences
    },
    detailRandomAccess: {
      openReads: wasmOpenDetailReads.length,
      openBytes: wasmOpenDetailReads.reduce((sum, value) => sum + value.byteLength, 0),
      openMaximumReadBytes: Math.max(...wasmOpenDetailReads.map(value => value.byteLength)),
      reads: wasmDetails.reads.length,
      bytes: wasmDetails.reads.reduce((sum, value) => sum + value.byteLength, 0),
      maximumReadBytes: Math.max(...wasmDetails.reads.map(value => value.byteLength)),
      wholeFileRead: wasmOpenDetailReads.reduce((sum, value) => sum + value.byteLength, 0)
        >= detailFile.size
    },
    allowlist: { entries: 0 }
  };
  console.log(JSON.stringify(report, null, 2));
  if (
    rawExact !== 1_236
    || romanizationStats.exact !== 5
    || detailedExact !== 702
    || (mode === 'immutable-baseline' && fallbackExact !== 301)
    || (mode === 'same-pack' && (
      report.detailRandomAccess.openReads !== 2
      || report.detailRandomAccess.wholeFileRead
    ))
  ) {
    process.exitCode = 1;
  }
}

if (import.meta.main) await main();
