import type { BrowserAnalyzer, AnalyzerRelease } from './analyzer-service.js';
import type {
  AnalysisResult,
  AnalyzeOptions,
  BenchmarkGroupResult,
  BenchmarkResult
} from './protocol.js';

declare global {
  interface Window {
    __ichiranQualification?: {
      analyze(text: string, options: AnalyzeOptions): Promise<AnalysisResult>;
      benchmark(): Promise<BenchmarkResult>;
      romanize(text: string): Promise<string>;
    };
  }
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

/** Attaches qualification checks to the product's one analyzer instance. */
export function attachQualificationBridge(
  analyzer: BrowserAnalyzer,
  release: AnalyzerRelease
): () => void {
  window.__ichiranQualification = {
    analyze: (text, options) => analyzer.analyze(text, options),
    benchmark: () => benchmarkAnalyzer(analyzer, release),
    romanize: text => analyzer.romanize(text)
  };
  return () => { delete window.__ichiranQualification; };
}

/** Complete UI-to-Worker benchmark, compiled only into qualification builds. */
async function benchmarkAnalyzer(
  analyzer: BrowserAnalyzer,
  release: AnalyzerRelease
): Promise<BenchmarkResult> {
  const readyStarted = performance.now();
  await analyzer.status();
  const workerReadyMs = performance.now() - readyStarted;
  const { default: corpus } = await import('./generated/benchmark-corpus.json');
  const requests = (values: readonly (readonly unknown[])[]): readonly Request[] => values.map(value => {
    const [text, limit = 1, entities] = value as CorpusRequest;
    return { text, limit, entities };
  });
  const first = requests(corpus.groups.ordinary)[0];
  if (!first) throw new Error('Benchmark corpus has no ordinary request');
  const firstStarted = performance.now();
  await analyzer.analyze(first.text, { limit: 1 });
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
      analyzer,
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
      analyzer,
      name,
      values,
      0x1c42_0000 + index * 100,
      0x1c42_1000 + index * 100,
      false
    ));
  }

  const entryIndices: number[] = [];
  for (const request of requests(corpus.groups['describe-random-access'])) {
    const result = await analyzer.analyze(request.text, { limit: 1 });
    const entryIndex = result.paths[0]?.tokens.find(token => token.entryIndex !== null)?.entryIndex;
    if (entryIndex === null || entryIndex === undefined) {
      throw new Error(`Entry benchmark probe has no dictionary entry: ${request.text}`);
    }
    entryIndices.push(entryIndex);
  }
  const describe = await measureEntryGroup(analyzer, entryIndices);

  return {
    release,
    corpusVersion: 3,
    warmupPasses: 2,
    measuredPasses: 10,
    groups,
    diagnostics: { analyzeGroups, describe, workerReadyMs, firstAnalyzeMs }
  };
}

async function measureAnalyzeGroup(
  analyzer: BrowserAnalyzer,
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
      await analyzer.analyze(request.text, options(request));
    }
  }
  const rawMs: number[] = [];
  for (let pass = 0; pass < 10; pass++) {
    for (const request of shuffled(requests, measuredSeed + pass)) {
      const started = performance.now();
      await analyzer.analyze(request.text, options(request));
      rawMs.push(performance.now() - started);
    }
  }
  return summarize(name, rawMs);
}

async function measureEntryGroup(
  analyzer: BrowserAnalyzer,
  entryIndices: readonly number[]
): Promise<BenchmarkGroupResult> {
  for (let pass = 0; pass < 2; pass++) {
    for (const entryIndex of shuffled(entryIndices, 0x1c43_0000 + pass)) {
      await analyzer.entry(entryIndex);
    }
  }
  const rawMs: number[] = [];
  for (let pass = 0; pass < 10; pass++) {
    for (const entryIndex of shuffled(entryIndices, 0x1c43_1000 + pass)) {
      const started = performance.now();
      await analyzer.entry(entryIndex);
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
