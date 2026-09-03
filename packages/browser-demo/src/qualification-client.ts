import type { AnalyzerDiagnostics } from '@ichiran/core/qualification/runtime';
import type {
  AnalyzeOptions,
  AnalysisResult,
  AnalyzerPackManifest,
  DictionaryEntry,
  PackStatus
} from './protocol.js';
import {
  requestClientInternal,
  type AnalyzerClient
} from './client.js';

export interface BenchmarkGroupResult {
  readonly corpus: string;
  readonly samples: number;
  readonly p50Ms: number;
  readonly p95Ms: number;
  readonly maxMs: number;
  readonly rawMs: readonly number[];
}

export interface BenchmarkResult {
  readonly release: AnalyzerPackManifest;
  readonly corpusVersion: 3;
  readonly warmupPasses: 2;
  readonly measuredPasses: 10;
  readonly groups: readonly BenchmarkGroupResult[];
  readonly diagnostics: {
    readonly analyzeGroups: readonly BenchmarkGroupResult[];
    readonly entry: BenchmarkGroupResult;
    readonly workerReadyMs: number;
    readonly firstAnalyzeMs: number;
  };
}

type CorpusRequest = readonly [
  text: string,
  limit?: number,
  entities?: AnalyzeOptions['entities']
];

interface Request {
  readonly text: string;
  readonly limit: number;
  readonly entities?: AnalyzeOptions['entities'];
}

/** Qualification-only facade over the App's existing client and Worker. */
export interface AnalyzerQualification {
  benchmark(): Promise<BenchmarkResult>;
  status(): Promise<PackStatus>;
  analyze(text: string, options?: AnalyzeOptions): Promise<AnalysisResult>;
  entry(entryIndex: number): Promise<DictionaryEntry>;
  diagnostics(): Promise<AnalyzerDiagnostics>;
}

export function createAnalyzerQualification(
  client: AnalyzerClient,
  release: AnalyzerPackManifest
): AnalyzerQualification {
  return {
    benchmark: () => benchmarkAnalyzer(client, release),
    status: () => client.status(),
    analyze: (text, options) => client.analyze(text, options),
    entry: entryIndex => client.entry(entryIndex),
    diagnostics: () => requestClientInternal<AnalyzerDiagnostics>(client, {
      op: 'rust-kernel-metrics'
    })
  };
}

/** Complete client-to-Worker benchmark used only by release qualification. */
export async function benchmarkAnalyzer(
  client: AnalyzerClient,
  release: AnalyzerPackManifest
): Promise<BenchmarkResult> {
  const readyStarted = performance.now();
  await client.status();
  const workerReadyMs = performance.now() - readyStarted;
  const { default: corpus } = await import('./generated/benchmark-corpus.json');
  const requests = (values: readonly (readonly unknown[])[]): readonly Request[] => values.map(value => {
    const [text, limit = 1, entities] = value as CorpusRequest;
    return { text, limit, entities };
  });
  const first = requests(corpus.groups.ordinary)[0];
  if (!first) throw new Error('Benchmark corpus has no ordinary request');
  const firstStarted = performance.now();
  await client.analyze(first.text, { limit: 1 });
  const firstAnalyzeMs = performance.now() - firstStarted;

  const hardGroups = [
    ['ordinary', requests(corpus.groups.ordinary), true],
    ['pathological-morphology', requests(corpus.groups['pathological-morphology']), true],
    ['dense-contiguous-boundary', requests(corpus.groups['dense-contiguous-boundary']), false]
  ] as const;
  const groups: BenchmarkGroupResult[] = [];
  for (let index = 0; index < hardGroups.length; index++) {
    const [name, values, forceTopOne] = hardGroups[index]!;
    groups.push(await measureAnalyzeGroup(
      client,
      name,
      values,
      0x1c41_0000 + index * 100,
      0x1c41_1000 + index * 100,
      forceTopOne
    ));
  }

  const diagnosticGroups: readonly (readonly [string, readonly Request[]])[] = [
    ['segmentation-short', requests(corpus.groups['segmentation-short'])],
    ['long-noun-compound', requests(corpus.groups['long-noun-compound'])],
    ['hiragana-colloquial', requests(corpus.groups['hiragana-colloquial'])],
    ['modern-mixed-script', requests(corpus.groups['modern-mixed-script'])],
    ['top-n', requests(corpus.groups['top-n'])],
    ['entities', requests(corpus.groups.entities)],
    ['counters', requests(corpus.groups.counters)],
    ['numbers', requests(corpus.groups.numbers)],
    ['paragraph-scaling', requests(corpus.groups['paragraph-scaling'])]
  ];
  const analyzeGroups: BenchmarkGroupResult[] = [];
  for (let index = 0; index < diagnosticGroups.length; index++) {
    const [name, values] = diagnosticGroups[index]!;
    analyzeGroups.push(await measureAnalyzeGroup(
      client,
      name,
      values,
      0x1c42_0000 + index * 100,
      0x1c42_1000 + index * 100,
      false
    ));
  }

  const entryIndices: number[] = [];
  for (const request of requests(corpus.groups['describe-random-access'])) {
    const result = await client.analyze(request.text, { limit: 1 });
    const entryIndex = result.paths[0]?.tokens.find(token => token.entryIndex !== null)?.entryIndex;
    if (entryIndex === null || entryIndex === undefined) {
      throw new Error(`Entry benchmark probe has no dictionary entry: ${request.text}`);
    }
    entryIndices.push(entryIndex);
  }
  const entry = await measureEntryGroup(client, entryIndices);

  return {
    release,
    corpusVersion: 3,
    warmupPasses: 2,
    measuredPasses: 10,
    groups,
    diagnostics: { analyzeGroups, entry, workerReadyMs, firstAnalyzeMs }
  };
}

async function measureAnalyzeGroup(
  client: AnalyzerClient,
  name: string,
  requests: readonly Request[],
  warmupSeed: number,
  measuredSeed: number,
  forceTopOne: boolean
): Promise<BenchmarkGroupResult> {
  const options = (request: Request): AnalyzeOptions => forceTopOne
    ? { limit: 1 }
    : {
        limit: request.limit,
        ...(request.entities === undefined ? {} : { entities: request.entities })
      };
  for (let pass = 0; pass < 2; pass++) {
    for (const request of shuffled(requests, warmupSeed + pass)) {
      await client.analyze(request.text, options(request));
    }
  }
  const rawMs: number[] = [];
  for (let pass = 0; pass < 10; pass++) {
    for (const request of shuffled(requests, measuredSeed + pass)) {
      const started = performance.now();
      await client.analyze(request.text, options(request));
      rawMs.push(performance.now() - started);
    }
  }
  return summarize(name, rawMs);
}

async function measureEntryGroup(
  client: AnalyzerClient,
  entryIndices: readonly number[]
): Promise<BenchmarkGroupResult> {
  for (let pass = 0; pass < 2; pass++) {
    for (const entryIndex of shuffled(entryIndices, 0x1c43_0000 + pass)) {
      await client.entry(entryIndex);
    }
  }
  const rawMs: number[] = [];
  for (let pass = 0; pass < 10; pass++) {
    for (const entryIndex of shuffled(entryIndices, 0x1c43_1000 + pass)) {
      const started = performance.now();
      await client.entry(entryIndex);
      rawMs.push(performance.now() - started);
    }
  }
  return summarize('entry-random-access', rawMs);
}

function shuffled<T>(values: readonly T[], seed: number): T[] {
  const output = [...values];
  let state = seed >>> 0;
  const random = (): number => {
    state += 0x6d2b79f5;
    let value = state;
    value = Math.imul(value ^ value >>> 15, value | 1);
    value ^= value + Math.imul(value ^ value >>> 7, value | 61);
    return ((value ^ value >>> 14) >>> 0) / 0x1_0000_0000;
  };
  for (let index = output.length - 1; index > 0; index--) {
    const target = Math.floor(random() * (index + 1));
    [output[index], output[target]] = [output[target]!, output[index]!];
  }
  return output;
}

function summarize(corpus: string, rawMs: readonly number[]): BenchmarkGroupResult {
  const ordered = [...rawMs].sort((left, right) => left - right);
  const rank = (quantile: number): number => ordered.length === 0
    ? 0
    : ordered[Math.max(0, Math.ceil(quantile * ordered.length) - 1)]!;
  return {
    corpus,
    samples: rawMs.length,
    p50Ms: rank(0.5),
    p95Ms: rank(0.95),
    maxMs: ordered.at(-1) ?? 0,
    rawMs
  };
}
