import type {
  AnalyzeOptions,
  AnalysisResult,
  AnalyzerPackManifest,
  BenchmarkResult,
  InstallPhase,
  PackStatus,
  WorkerRequest,
  WorkerResponse
} from './protocol.js';
import { parseAnalyzerReleaseManifest } from '@ichiran/core';
import { Sha256 } from './worker/sha256.js';

export function parseDeployedRelease(value: unknown): AnalyzerPackManifest {
  return parseAnalyzerReleaseManifest(
    value,
    text => new Sha256().update(new TextEncoder().encode(text)).digestHex()
  );
}

export interface InstallProgressValue {
  readonly phase: InstallPhase;
  readonly receivedBytes: number;
  readonly totalBytes: number;
}

export class AnalyzerClientError extends Error {
  readonly code: string;

  constructor(code: string, message: string) {
    super(message);
    this.name = 'AnalyzerClientError';
    this.code = code;
  }
}

interface PendingRequest {
  readonly resolve: (value: unknown) => void;
  readonly reject: (error: Error) => void;
  readonly progress?: (value: InstallProgressValue) => void;
}

interface BenchmarkRequest {
  readonly text: string;
  readonly limit: number;
  readonly entities?: AnalyzeOptions['entities'];
}

type WorkerRequestBody = WorkerRequest extends infer Request
  ? Request extends { readonly id: number }
    ? Omit<Request, 'id'>
    : never
  : never;

type AnalyzerWorkerFactory = () => Worker;

function createAnalyzerWorker(): Worker {
  return new Worker(new URL('./analyzer.worker.ts', import.meta.url), {
    type: 'module',
    name: 'ichiran-analyzer'
  });
}

/** One thin request map around the dedicated analyzer Worker. */
export class AnalyzerClient {
  #worker: Worker | null = null;
  #expectedRelease: AnalyzerPackManifest | null = null;
  #initializedWorker: Worker | null = null;
  #initialization: { readonly worker: Worker; readonly promise: Promise<PackStatus> } | null = null;
  #disposed = false;
  #nextId = 1;
  #workerReadyMs: number | null = null;
  #firstAnalyzeMs: number | null = null;
  #firstAnalyzePending = false;
  readonly #workerFactory: AnalyzerWorkerFactory;
  readonly #pending = new Map<number, PendingRequest>();

  constructor(workerFactory: AnalyzerWorkerFactory = createAnalyzerWorker) {
    this.#workerFactory = workerFactory;
  }

  /** Stops obsolete work so a newer user request does not wait behind it. */
  restart(): void {
    if (this.#disposed) return;
    this.#replaceWorker(new AnalyzerClientError(
      'request-superseded',
      'Analyzer request was replaced by newer input'
    ));
  }

  /** Pins every Worker incarnation to the authenticated release deployed with this shell. */
  async expectRelease(release: AnalyzerPackManifest): Promise<PackStatus> {
    const started = performance.now();
    const verifiedRelease = parseDeployedRelease(release);
    if (
      this.#expectedRelease
      && this.#expectedRelease.manifestSha256 !== verifiedRelease.manifestSha256
    ) {
      this.#replaceWorker(new AnalyzerClientError(
        'release-changed',
        'The deployed analyzer release changed. Restarting the analyzer.'
      ));
    }
    this.#expectedRelease = verifiedRelease;
    const worker = this.#openWorker();
    const initialized = await this.#initializeWorker(worker);
    if (worker !== this.#worker) {
      throw new AnalyzerClientError(
        'worker-crashed',
        'Analyzer Worker stopped unexpectedly. Try again to restart it.'
      );
    }
    const status = initialized ?? await this.#requestOnWorker<PackStatus>(worker, { op: 'status' });
    if (status.state === 'ready' && this.#workerReadyMs === null) {
      this.#workerReadyMs = performance.now() - started;
    }
    return status;
  }

  async status(): Promise<PackStatus> {
    const started = performance.now();
    const status = await this.#request<PackStatus>({ op: 'status' });
    if (status.state === 'ready' && this.#workerReadyMs === null) {
      this.#workerReadyMs = performance.now() - started;
    }
    return status;
  }

  install(
    manifestUrl: string,
    progress: (value: InstallProgressValue) => void
  ): Promise<PackStatus> {
    return this.#request({ op: 'install', manifestUrl }, progress);
  }

  clear(): Promise<PackStatus> {
    return this.#request({ op: 'clear' });
  }

  async analyze(text: string, options: AnalyzeOptions): Promise<AnalysisResult> {
    const measureFirst = this.#firstAnalyzeMs === null && !this.#firstAnalyzePending;
    const started = measureFirst ? performance.now() : 0;
    if (measureFirst) this.#firstAnalyzePending = true;
    try {
      const result = await this.#request<AnalysisResult>({ op: 'analyze', text, options });
      if (measureFirst) this.#firstAnalyzeMs = performance.now() - started;
      return result;
    } finally {
      if (measureFirst) this.#firstAnalyzePending = false;
    }
  }

  legacy(text: string, options: AnalyzeOptions): Promise<unknown> {
    return this.#request({ op: 'legacy', text, options });
  }

  describe(entryIndex: number): Promise<unknown> {
    return this.#request({ op: 'describe', entryIndex });
  }

  romanize(text: string): Promise<string> {
    return this.#request({ op: 'romanize', text });
  }

  /** Measures the complete UI-to-Worker RPC, including result cloning. */
  async benchmark(release: AnalyzerPackManifest): Promise<BenchmarkResult> {
    const { default: benchmarkCorpus } = await import('../../../browser-alpha/bench/corpus.json');
    const hardGroups = [
      ['ordinary', benchmarkCorpus.groups.ordinary, true],
      ['pathological-morphology', benchmarkCorpus.groups['pathological-morphology'], true],
      [
        'dense-contiguous-boundary',
        benchmarkCorpus.groups['dense-contiguous-boundary'],
        false
      ]
    ] as const;
    const results: BenchmarkResult['groups'][number][] = [];
    for (let groupIndex = 0; groupIndex < hardGroups.length; groupIndex++) {
      const [name, requests, forceTopOne] = hardGroups[groupIndex]!;
      results.push(await this.#measureAnalyzeGroup(
        name,
        requests,
        0x1c41_0000 + groupIndex * 100,
        0x1c41_1000 + groupIndex * 100,
        forceTopOne
      ));
    }

    const diagnosticGroups: readonly (readonly [string, readonly BenchmarkRequest[]])[] = [
      ['segmentation-short', benchmarkCorpus.groups['segmentation-short']],
      ['long-noun-compound', benchmarkCorpus.groups['long-noun-compound']],
      ['hiragana-colloquial', benchmarkCorpus.groups['hiragana-colloquial']],
      ['modern-mixed-script', benchmarkCorpus.groups['modern-mixed-script']],
      ['top-n', benchmarkCorpus.groups['top-n']],
      ['entities', benchmarkCorpus.groups.entities],
      ['counters', benchmarkCorpus.groups.counters],
      ['numbers', benchmarkCorpus.groups.numbers],
      ['paragraph-scaling', benchmarkCorpus.groups['paragraph-scaling']]
    ];
    const diagnosticResults: BenchmarkResult['diagnostics']['analyzeGroups'][number][] = [];
    for (let groupIndex = 0; groupIndex < diagnosticGroups.length; groupIndex++) {
      const [name, requests] = diagnosticGroups[groupIndex]!;
      diagnosticResults.push(await this.#measureAnalyzeGroup(
        name,
        requests,
        0x1c42_0000 + groupIndex * 100,
        0x1c42_1000 + groupIndex * 100,
        false
      ));
    }

    const detailEntries: number[] = [];
    for (const request of benchmarkCorpus.groups['describe-random-access']) {
      const result = await this.analyze(request.text, { limit: 1 });
      const entryIndex = result.paths[0]?.tokens.find(token => token.entryIndex !== null)?.entryIndex;
      if (entryIndex === null || entryIndex === undefined) {
        throw new Error(`Describe benchmark probe has no dictionary entry: ${request.text}`);
      }
      detailEntries.push(entryIndex);
    }
    const describe = await this.#measureDescribeGroup(detailEntries);

    return {
      release,
      corpusVersion: 3,
      warmupPasses: 2,
      measuredPasses: 10,
      groups: results,
      diagnostics: {
        analyzeGroups: diagnosticResults,
        describe,
        workerReadyMs: this.#workerReadyMs,
        firstAnalyzeMs: this.#firstAnalyzeMs
      }
    };
  }

  async #measureAnalyzeGroup(
    name: string,
    requests: readonly BenchmarkRequest[],
    warmupSeed: number,
    measuredSeed: number,
    forceTopOne: boolean
  ): Promise<BenchmarkResult['groups'][number]> {
    const options = (request: BenchmarkRequest): AnalyzeOptions => forceTopOne
      ? { limit: 1 }
      : {
          limit: request.limit,
          ...(request.entities === undefined ? {} : { entities: request.entities })
        };
    for (let pass = 0; pass < 2; pass++) {
      for (const request of shuffled(requests, warmupSeed + pass)) {
        await this.analyze(request.text, options(request));
      }
    }
    const rawMs: number[] = [];
    for (let pass = 0; pass < 10; pass++) {
      for (const request of shuffled(requests, measuredSeed + pass)) {
        const started = performance.now();
        await this.analyze(request.text, options(request));
        rawMs.push(performance.now() - started);
      }
    }
    return summarize(name, rawMs);
  }

  async #measureDescribeGroup(
    entryIndices: readonly number[]
  ): Promise<BenchmarkResult['diagnostics']['describe']> {
    for (let pass = 0; pass < 2; pass++) {
      for (const entryIndex of shuffled(entryIndices, 0x1c43_0000 + pass)) {
        await this.describe(entryIndex);
      }
    }
    const rawMs: number[] = [];
    for (let pass = 0; pass < 10; pass++) {
      for (const entryIndex of shuffled(entryIndices, 0x1c43_1000 + pass)) {
        const started = performance.now();
        await this.describe(entryIndex);
        rawMs.push(performance.now() - started);
      }
    }
    return summarize('describe-random-access', rawMs);
  }

  dispose(): void {
    if (this.#disposed) return;
    this.#disposed = true;
    this.#worker?.terminate();
    this.#worker = null;
    const error = new AnalyzerClientError('worker-terminated', 'Analyzer Worker was stopped');
    for (const pending of this.#pending.values()) pending.reject(error);
    this.#pending.clear();
  }

  #createWorker(): Worker {
    const worker = this.#workerFactory();
    worker.addEventListener('message', (event: MessageEvent<WorkerResponse>) => {
      if (worker !== this.#worker) return;
      const response = event.data;
      const pending = this.#pending.get(response.id);
      if (!pending) return;
      if (response.type === 'progress') {
        pending.progress?.({
          phase: response.phase,
          receivedBytes: response.receivedBytes,
          totalBytes: response.totalBytes
        });
        return;
      }
      this.#pending.delete(response.id);
      if (response.type === 'error') {
        pending.reject(new AnalyzerClientError(response.code, response.message));
      } else {
        pending.resolve(response.result);
      }
    });
    worker.addEventListener('error', event => {
      event.preventDefault();
      if (worker !== this.#worker) return;
      this.#replaceWorker(new AnalyzerClientError(
        'worker-crashed',
        'Analyzer Worker stopped unexpectedly. Try again to restart it.'
      ));
    });
    worker.addEventListener('messageerror', () => {
      if (worker !== this.#worker) return;
      this.#replaceWorker(new AnalyzerClientError(
        'worker-crashed',
        'Analyzer Worker returned unreadable data. Try again to restart it.'
      ));
    });
    return worker;
  }

  #replaceWorker(reason: AnalyzerClientError): void {
    const previous = this.#worker;
    this.#worker = null;
    previous?.terminate();
    for (const pending of this.#pending.values()) pending.reject(reason);
    this.#pending.clear();
    this.#workerReadyMs = null;
    this.#firstAnalyzeMs = null;
    this.#firstAnalyzePending = false;
    this.#initializedWorker = null;
    this.#initialization = null;
  }

  #openWorker(): Worker {
    if (this.#disposed) {
      throw new AnalyzerClientError('worker-terminated', 'Analyzer Worker was stopped');
    }
    if (this.#worker) return this.#worker;
    try {
      this.#worker = this.#createWorker();
      return this.#worker;
    } catch {
      throw new AnalyzerClientError(
        'worker-unavailable',
        'The analyzer could not restart. Reload this page and try again.'
      );
    }
  }

  async #initializeWorker(worker: Worker): Promise<PackStatus | null> {
    if (this.#initializedWorker === worker) return null;
    if (this.#initialization?.worker === worker) return this.#initialization.promise;
    const release = this.#expectedRelease;
    if (!release) {
      throw new AnalyzerClientError(
        'release-not-set',
        'The deployed analyzer release is not available. Reload this page and try again.'
      );
    }
    const promise = this.#requestOnWorker<PackStatus>(worker, {
      op: 'expect-release',
      release
    }).then(status => {
      if (worker === this.#worker && release === this.#expectedRelease) {
        this.#initializedWorker = worker;
      }
      return status;
    }).finally(() => {
      if (this.#initialization?.promise === promise) this.#initialization = null;
    });
    this.#initialization = { worker, promise };
    return promise;
  }

  async #request<T>(
    body: WorkerRequestBody,
    progress?: (value: InstallProgressValue) => void
  ): Promise<T> {
    const worker = this.#openWorker();
    if (this.#initializedWorker === worker) {
      return this.#requestOnWorker(worker, body, progress);
    }
    const initialized = await this.#initializeWorker(worker);
    if (body.op === 'status' && initialized) return initialized as T;
    if (worker !== this.#worker) {
      throw new AnalyzerClientError(
        'worker-crashed',
        'Analyzer Worker stopped unexpectedly. Try again to restart it.'
      );
    }
    return this.#requestOnWorker(worker, body, progress);
  }

  #requestOnWorker<T>(
    worker: Worker,
    body: WorkerRequestBody,
    progress?: (value: InstallProgressValue) => void
  ): Promise<T> {
    const id = this.#nextId++;
    return new Promise<T>((resolve, reject) => {
      this.#pending.set(id, {
        resolve: (value) => resolve(value as T),
        reject,
        progress
      });
      try {
        worker.postMessage({ id, ...body } as WorkerRequest);
      } catch (reason) {
        this.#pending.delete(id);
        reject(reason instanceof Error ? reason : new Error(String(reason)));
      }
    });
  }
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

function nearestRank(ordered: readonly number[], quantile: number): number {
  if (ordered.length === 0) return 0;
  return ordered[Math.max(0, Math.ceil(quantile * ordered.length) - 1)]!;
}

function summarize(corpus: string, rawMs: readonly number[]): BenchmarkResult['groups'][number] {
  const ordered = [...rawMs].sort((left, right) => left - right);
  return {
    corpus,
    samples: rawMs.length,
    p50Ms: nearestRank(ordered, 0.5),
    p95Ms: nearestRank(ordered, 0.95),
    maxMs: ordered[ordered.length - 1] ?? 0,
    rawMs
  };
}
