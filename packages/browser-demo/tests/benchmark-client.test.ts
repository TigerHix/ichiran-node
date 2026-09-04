import { afterEach, describe, expect, test } from 'bun:test';
import { analyzerManifestDigestInput } from '@ichiran/core/release';

import { AnalyzerClient } from '../src/client.js';
import { benchmarkAnalyzer } from '../src/qualification-client.js';
import type { AnalyzerPackManifest, WorkerRequest, WorkerResponse } from '../src/protocol.js';
import { Sha256 } from '../src/worker/sha256.js';

class BenchmarkWorker {
  static requests: WorkerRequest[] = [];

  readonly #listeners = new Map<string, ((event: MessageEvent<WorkerResponse>) => void)[]>();

  addEventListener(type: string, listener: EventListenerOrEventListenerObject): void {
    const callback = typeof listener === 'function'
      ? listener as (event: MessageEvent<WorkerResponse>) => void
      : event => listener.handleEvent(event);
    this.#listeners.set(type, [...this.#listeners.get(type) ?? [], callback]);
  }

  postMessage(request: WorkerRequest): void {
    BenchmarkWorker.requests.push(request);
    let result: unknown;
    if (request.op === 'status' || request.op === 'expect-release') {
      result = {
        state: 'ready',
        packVersion: 'test',
        manifestSha256: 'a'.repeat(64),
        downloadBytes: 2,
        installedBytes: 2,
        persistent: false,
        workerOpen: true
      };
    } else if (request.op === 'analyze') {
      result = {
        input: request.text,
        normalized: request.text,
        computeMs: 0,
        paths: [{
          score: 0,
          tokens: [{ entryIndex: request.text.codePointAt(0) ?? 0 }]
        }]
      };
    } else {
      result = {};
    }
    const response: WorkerResponse = { id: request.id, type: 'result', result };
    for (const listener of this.#listeners.get('message') ?? []) {
      listener({ data: response } as MessageEvent<WorkerResponse>);
    }
  }

  terminate(): void {}
}

const unsignedRelease = {
  formatVersion: 2,
  packVersion: 'test',
  sourceCommit: 'b'.repeat(40),
  sourcesLockSha256: 'c'.repeat(64),
  hot: {
    file: 'hot.bin.gz', encoding: 'gzip', downloadBytes: 1,
    downloadSha256: 'd'.repeat(64), installedBytes: 1, installedSha256: 'e'.repeat(64)
  },
  lexicon: {
    file: 'lexicon.bin.gz', encoding: 'gzip', downloadBytes: 1,
    downloadSha256: 'f'.repeat(64), installedBytes: 1, installedSha256: '0'.repeat(64)
  },
  locales: {
    en: {
      file: 'gloss.en.bin.gz', encoding: 'gzip', downloadBytes: 1,
      downloadSha256: '1'.repeat(64), installedBytes: 1, installedSha256: '2'.repeat(64)
    },
    'zh-Hans': {
      file: 'gloss.zh-Hans.bin.gz', encoding: 'gzip', downloadBytes: 1,
      downloadSha256: '3'.repeat(64), installedBytes: 1, installedSha256: '4'.repeat(64)
    }
  }
} as const;
const release: AnalyzerPackManifest = {
  ...unsignedRelease,
  manifestSha256: new Sha256()
    .update(new TextEncoder().encode(analyzerManifestDigestInput(unsignedRelease)))
    .digestHex()
};

afterEach(() => {
  BenchmarkWorker.requests = [];
});

describe('qualification-only Worker benchmark report', () => {
  test('keeps hard groups separate and reports every diagnostic without gating it', async () => {
    const client = new AnalyzerClient(
      () => new BenchmarkWorker() as unknown as Worker
    );
    await client.expectRelease(release);
    const report = await benchmarkAnalyzer(client, release);
    client.dispose();

    expect(report.corpusVersion).toBe(3);
    expect(report.groups.map(group => [group.corpus, group.samples])).toEqual([
      ['ordinary', 990],
      ['pathological-morphology', 500],
      ['dense-contiguous-boundary', 120]
    ]);
    expect(report.diagnostics.analyzeGroups.map(group => [group.corpus, group.samples])).toEqual([
      ['segmentation-short', 4590],
      ['long-noun-compound', 500],
      ['hiragana-colloquial', 500],
      ['modern-mixed-script', 500],
      ['top-n', 20],
      ['entities', 540],
      ['counters', 2000],
      ['numbers', 70],
      ['paragraph-scaling', 50]
    ]);
    expect(report.diagnostics.entry.corpus).toBe('entry-random-access');
    expect(report.diagnostics.entry.samples).toBe(500);
    expect(report.diagnostics.workerReadyMs).toBeNumber();
    expect(report.diagnostics.firstAnalyzeMs).toBeNumber();

    const analyzes = BenchmarkWorker.requests.filter(
      (request): request is Extract<WorkerRequest, { op: 'analyze' }> => request.op === 'analyze'
    );
    expect(analyzes.filter(request => request.options.limit === 3)).toHaveLength(12);
    expect(analyzes.filter(request => request.options.limit === 5)).toHaveLength(5 * 12);
    expect(analyzes.filter(request => request.options.limit === 10)).toHaveLength(9 * 12);
    expect(analyzes.filter(request => request.options.entities !== undefined)).toHaveLength(54 * 12);
    expect(BenchmarkWorker.requests.filter(request => request.op === 'entry')).toHaveLength(50 * 12);
  });
});
