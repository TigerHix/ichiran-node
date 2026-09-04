#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { createReadStream } from 'node:fs';
import { readFile, stat } from 'node:fs/promises';
import { join, resolve } from 'node:path';

import { Analyzer, type AnalysisResult, type AnalyzeOptions } from '../src/index.js';
import { parseAnalyzerReleaseManifest } from '../src/release.js';
import { legacyAnalysis } from '../src/runtime-qualification.js';
import { TypeScriptOracleRuntime } from '../src/runtime-typescript.js';
import {
  projectPortableCleanAnalysis,
  type CanonicalDifference,
  type CleanAnalysisResult
} from './parity-canonical.js';
import {
  type AnalyzerFixtureRequest,
  type AnalyzerParityCorpus
} from './parity-corpus.js';

type SuiteName = 'segmentation' | 'cli' | 'hard' | 'counters' | 'entities' | 'probes';
type DetailedSuiteName = Exclude<SuiteName, 'segmentation'>;

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
  readonly request: BatchRequest;
}

interface FallbackCase {
  readonly request: AnalyzerFixtureRequest;
  readonly entities?: readonly BatchEntity[];
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

interface TrackedSource {
  readonly byteLength: number;
  readonly reads: Array<{ readonly offset: number; readonly byteLength: number }>;
  read(offset: number, byteLength: number): Promise<Uint8Array>;
}

function digest(bytes: Uint8Array | string): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function fileDigest(path: string): Promise<string> {
  const hash = createHash('sha256');
  for await (const chunk of createReadStream(path)) hash.update(chunk);
  return hash.digest('hex');
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

function options(request: BatchRequest): AnalyzeOptions {
  return {
    limit: request.limit,
    ...(request.entities ? { entities: request.entities } : {}),
    normalizePunctuation: request.normalizePunctuation
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

/** Same-pack qualification ignores elapsed time and compares every other public DTO field. */
export function samePackAnalysisDifference(
  expected: AnalysisResult,
  actual: AnalysisResult
): CanonicalDifference | null {
  return rawDifference({ ...expected, computeMs: 0 }, { ...actual, computeMs: 0 });
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
    { text: value.text, limit: 1 }, value.entities
  )));
  append('probes', corpus.probes.map(value => batchRequest(value.request)));
  if (result.length !== 1_236) {
    throw new Error(`raw WASM corpus has ${result.length} requests; expected 1236`);
  }
  return result;
}

function fallbackRequests(value: unknown): Record<'counters' | 'entities' | 'probes', FallbackCase[]> {
  const fixture = value as {
    readonly formatVersion?: number;
    readonly counts?: Record<string, number>;
    readonly suites?: Record<string, FallbackCase[]>;
  };
  if (fixture.formatVersion !== 1 || fixture.counts?.total !== 301 || !fixture.suites) {
    throw new Error('same-pack request fixture identity or accounting is invalid');
  }
  return {
    counters: fixture.suites.counters,
    entities: fixture.suites.entities,
    probes: fixture.suites.probes
  };
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
  const fixture = fallbackRequests(JSON.parse(new TextDecoder().decode(fixtureBytes)) as unknown);
  return {
    segmentation,
    romanization: cli.romanization,
    cli: cli.fullJson,
    hard: hard.fullJson,
    counters: fixture.counters.map(value => value.request),
    entities: fixture.entities.map((value, index) => ({
      title: `same-pack-${index}`,
      text: value.request.text,
      entities: value.entities ?? []
    })),
    probes: fixture.probes.map((value, index) => ({
      category: 'top-n',
      name: `same-pack-${index}`,
      request: value.request
    })),
    currentLispCli: {},
    currentLispHard: {},
    currentLispRomanization: {}
  };
}

function detailedCases(raw: readonly RawCase[]): DetailedCase[] {
  const result = raw.flatMap(value => value.suite === 'segmentation' ? [] : [{
    suite: value.suite,
    index: value.index,
    request: value.request
  }]);
  if (result.length !== 702) {
    throw new Error(`same-pack detailed corpus has ${result.length} requests; expected 702`);
  }
  return result;
}

function source(path: string): TrackedSource {
  const file = Bun.file(path);
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
  if (completed % 50 === 0 || completed === total) console.error(`${label}: ${completed}/${total}`);
}

function rootSequences(result: CleanAnalysisResult): Array<number | null> {
  return result.paths[0]?.tokens[0]?.alternatives.map(value => value.root?.seq ?? null) ?? [];
}

function installedPath(release: string, file: string): string {
  return join(release, file.replace(/\.gz$/, ''));
}

async function verifyInstalled(
  label: string,
  path: string,
  expectedBytes: number,
  expectedSha256: string
): Promise<void> {
  const [metadata, sha256] = await Promise.all([stat(path), fileDigest(path)]);
  if (metadata.size !== expectedBytes || sha256 !== expectedSha256) {
    throw new Error(
      `${label} is ${metadata.size} bytes with SHA-256 ${sha256}; manifest identifies `
      + `${expectedBytes} and ${expectedSha256}`
    );
  }
}

function randomAccessReport(
  path: string,
  sourceValue: TrackedSource,
  openReads: readonly { readonly offset: number; readonly byteLength: number }[]
) {
  const openBytes = openReads.reduce((sum, value) => sum + value.byteLength, 0);
  return {
    path,
    openReads: openReads.length,
    openBytes,
    openMaximumReadBytes: Math.max(0, ...openReads.map(value => value.byteLength)),
    reads: sourceValue.reads.length,
    bytes: sourceValue.reads.reduce((sum, value) => sum + value.byteLength, 0),
    maximumReadBytes: Math.max(0, ...sourceValue.reads.map(value => value.byteLength)),
    wholeFileReadAtOpen: openBytes >= sourceValue.byteLength
  };
}

async function main(): Promise<void> {
  const repository = resolve(import.meta.dir, '../../..');
  const arguments_ = process.argv.slice(2);
  if (arguments_[0] === '--same-pack') arguments_.shift();
  if (arguments_.length > 2) {
    throw new Error(
      'Usage: rust-kernel-wasm-differential.ts [--same-pack] [release-directory] [wasm-file]'
    );
  }
  const release = resolve(arguments_[0] ?? join(repository, 'browser-alpha/release'));
  const wasmPath = resolve(
    arguments_[1] ?? join(repository, 'packages/core/src/rust-kernel/generated/ichiran_kernel_bg.wasm')
  );
  const manifestPath = join(release, 'manifest.json');
  const fixturePath = join(repository, 'packages/rust-kernel/tests/fixtures/m3-fallback.json');
  const [manifestBytes, fixtureBytes, wasmBytes] = await Promise.all([
    readFile(manifestPath),
    readFile(fixturePath),
    readFile(wasmPath)
  ]);
  const manifest = parseAnalyzerReleaseManifest(
    JSON.parse(manifestBytes.toString('utf8')) as unknown,
    digest
  );
  const hotPath = installedPath(release, manifest.hot.file);
  const lexiconPath = installedPath(release, manifest.lexicon.file);
  const localePaths = Object.fromEntries(Object.entries(manifest.locales).map(([locale, asset]) => [
    locale,
    installedPath(release, asset.file)
  ]));
  if (!localePaths.en || !localePaths['zh-Hans']) {
    throw new Error('format-v2 same-pack qualification requires en and zh-Hans locale stores');
  }
  await Promise.all([
    verifyInstalled('hot', hotPath, manifest.hot.installedBytes, manifest.hot.installedSha256),
    verifyInstalled(
      'lexicon', lexiconPath, manifest.lexicon.installedBytes, manifest.lexicon.installedSha256
    ),
    ...Object.entries(manifest.locales).map(([locale, asset]) => verifyInstalled(
      `locale ${locale}`, localePaths[locale]!, asset.installedBytes, asset.installedSha256
    ))
  ]);
  const hotBytes = await readFile(hotPath);
  const corpus = await loadSamePackCorpus(repository, fixtureBytes);
  const raw = rawCases(corpus);
  const detailed = detailedCases(raw);

  const wasmLexicon = source(lexiconPath);
  const wasmLocales = Object.fromEntries(Object.entries(localePaths).map(([locale, path]) => [
    locale, source(path)
  ])) as Record<string, TrackedSource>;
  const oracleLexicon = source(lexiconPath);
  const oracleLocales = Object.fromEntries(Object.entries(localePaths).map(([locale, path]) => [
    locale, source(path)
  ])) as Record<string, TrackedSource>;
  const decodeGzip = async (
    compressed: Uint8Array,
    expectedByteLength: number
  ): Promise<Uint8Array> => {
    const decoded = new Uint8Array(Bun.gunzipSync(Uint8Array.from(compressed)));
    if (decoded.byteLength !== expectedByteLength) {
      throw new Error(`gzip decoded ${decoded.byteLength} bytes; expected ${expectedByteLength}`);
    }
    return decoded;
  };
  const oracle = await TypeScriptOracleRuntime.open({
    hot: new Uint8Array(hotBytes),
    lexicon: { source: oracleLexicon, sha256: manifest.lexicon.installedSha256 },
    locales: oracleLocales,
    decodeGzip
  });
  const wasm = await Analyzer.open({
    hot: new Uint8Array(hotBytes),
    lexicon: { source: wasmLexicon, sha256: manifest.lexicon.installedSha256 },
    locales: wasmLocales,
    wasm: new Uint8Array(wasmBytes)
  });
  const wasmOpenReads = {
    lexicon: [...wasmLexicon.reads],
    locales: Object.fromEntries(Object.entries(wasmLocales).map(([locale, value]) => [
      locale, [...value.reads]
    ])) as Record<string, readonly { readonly offset: number; readonly byteLength: number }[]>
  };

  const rawStats = stats(['segmentation', 'cli', 'hard', 'counters', 'entities', 'probes']);
  const detailedStats = stats(['cli', 'hard', 'counters', 'entities', 'probes']);
  const rawDifferences: unknown[] = [];
  const detailedDifferences: unknown[] = [];
  const localizedDifferences: unknown[] = [];
  const romanizationStats: ExactStats = { operations: 0, exact: 0, divergent: 0 };
  const romanizationDifferences: unknown[] = [];
  const wasmClean: CleanAnalysisResult[] = [];
  const oracleClean: CleanAnalysisResult[] = [];

  try {
    for (let index = 0; index < raw.length; index++) {
      const fixture = raw[index]!;
      const operationOptions = options(fixture.request);
      const [actual, expected] = await Promise.all([
        wasm.analyze(fixture.request.text, operationOptions),
        oracle.analyzeProduct(fixture.request.text, operationOptions)
      ]);
      wasmClean.push(projectPortableCleanAnalysis(actual));
      oracleClean.push(projectPortableCleanAnalysis(expected));
      const difference = samePackAnalysisDifference(expected, actual);
      const suite = rawStats[fixture.suite]!;
      suite.operations++;
      if (difference) {
        suite.divergent++;
        rawDifferences.push({ suite: fixture.suite, index: fixture.index, difference });
      } else suite.exact++;
      progress('WASM raw differential', index + 1, raw.length);
    }

    for (let index = 0; index < corpus.romanization.length; index++) {
      const input = corpus.romanization[index]!;
      const [actual, expected] = await Promise.all([wasm.romanize(input), oracle.romanize(input)]);
      const difference = rawDifference(expected, actual);
      romanizationStats.operations++;
      if (difference) {
        romanizationStats.divergent++;
        romanizationDifferences.push({ index, input, difference });
      } else romanizationStats.exact++;
    }

    for (let index = 0; index < detailed.length; index++) {
      const fixture = detailed[index]!;
      const [actual, expected] = await Promise.all([
        legacyAnalysis(wasm, fixture.request.text, options(fixture.request)),
        oracle.legacy(fixture.request.text, options(fixture.request))
      ]);
      const difference = rawDifference(expected, actual);
      const suite = detailedStats[fixture.suite]!;
      suite.operations++;
      if (difference) {
        suite.divergent++;
        detailedDifferences.push({ suite: fixture.suite, index: fixture.index, difference });
      } else suite.exact++;
      progress('WASM English dictionary differential', index + 1, detailed.length);
    }

    const localized = detailed.slice(0, 20);
    for (let index = 0; index < localized.length; index++) {
      const fixture = localized[index]!;
      const localizedOptions = { ...options(fixture.request), locale: 'zh-Hans' } as const;
      const [actual, expected] = await Promise.all([
        legacyAnalysis(wasm, fixture.request.text, localizedOptions),
        oracle.legacy(fixture.request.text, localizedOptions)
      ]);
      const difference = rawDifference(expected, actual);
      if (difference) localizedDifferences.push({ index, request: fixture.request, difference });
    }
  } finally {
    wasm.dispose();
  }

  const rawExact = Object.values(rawStats).reduce((sum, value) => sum + value.exact, 0);
  const detailedExact = Object.values(detailedStats).reduce((sum, value) => sum + value.exact, 0);
  const tieOffset = raw.findIndex(value => value.suite === 'probes' && value.request.text === 'ﾊｼ');
  if (tieOffset < 0) throw new Error('raw corpus has no halfwidth-katakana tie witness');
  const wasmTie = rootSequences(wasmClean[tieOffset]!);
  const oracleTie = rootSequences(oracleClean[tieOffset]!);
  const dictionaryRandomAccess = {
    lexicon: randomAccessReport(lexiconPath, wasmLexicon, wasmOpenReads.lexicon),
    locales: Object.fromEntries(Object.entries(wasmLocales).map(([locale, sourceValue]) => [
      locale,
      randomAccessReport(localePaths[locale]!, sourceValue, wasmOpenReads.locales[locale]!)
    ]))
  };
  const report = {
    formatVersion: 2,
    mode: 'same-pack',
    artifacts: {
      manifest: { path: manifestPath, sha256: digest(manifestBytes) },
      hot: { path: hotPath, bytes: hotBytes.byteLength, sha256: digest(hotBytes) },
      lexicon: {
        path: lexiconPath,
        bytes: manifest.lexicon.installedBytes,
        sha256: manifest.lexicon.installedSha256
      },
      locales: Object.fromEntries(Object.entries(manifest.locales).map(([locale, asset]) => [
        locale,
        { path: localePaths[locale], bytes: asset.installedBytes, sha256: asset.installedSha256 }
      ])),
      wasm: { path: wasmPath, bytes: wasmBytes.byteLength, sha256: digest(wasmBytes) }
    },
    rawAnalyzer: {
      policy: 'Complete public DTO equality against the TypeScript same-pack oracle after normalizing only computeMs.',
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
    standaloneRomanization: { ...romanizationStats, differences: romanizationDifferences },
    dictionaryLegacy: {
      locale: 'en',
      operations: detailed.length,
      exact: detailedExact,
      divergent: detailed.length - detailedExact,
      suites: detailedStats,
      differences: detailedDifferences
    },
    localizedSample: {
      locale: 'zh-Hans',
      operations: 20,
      exact: 20 - localizedDifferences.length,
      divergent: localizedDifferences.length,
      differences: localizedDifferences
    },
    dictionaryRandomAccess,
    allowlist: { entries: 0 }
  };
  console.log(JSON.stringify(report, null, 2));
  const allOpenReadsAreLazy = [
    dictionaryRandomAccess.lexicon,
    ...Object.values(dictionaryRandomAccess.locales)
  ].every(value => value.openReads === 2 && !value.wholeFileReadAtOpen);
  if (
    rawExact !== 1_236
    || romanizationStats.exact !== corpus.romanization.length
    || detailedExact !== 702
    || localizedDifferences.length !== 0
    || !allOpenReadsAreLazy
  ) process.exitCode = 1;
}

if (import.meta.main) await main();
