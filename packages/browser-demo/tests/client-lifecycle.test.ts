import { afterEach, describe, expect, test } from 'bun:test';
import { analyzerManifestDigestInput } from '@ichiran/core/release';

import { AnalyzerClient, AnalyzerClientError } from '../src/client.js';
import type {
  AnalyzerClientErrorCode,
  AnalyzerPackManifest,
  WorkerRequest,
  WorkerResponse
} from '../src/protocol.js';
import { Sha256 } from '../src/worker/sha256.js';

function sha256(text: string): string {
  return new Sha256().update(new TextEncoder().encode(text)).digestHex();
}

function release(packVersion = 'test-release'): AnalyzerPackManifest {
  const digest = 'a'.repeat(64);
  const unsigned = {
    formatVersion: 1 as const,
    packVersion,
    sourceCommit: 'b'.repeat(40),
    sourcesLockSha256: 'c'.repeat(64),
    hot: {
      file: 'hot.bin', encoding: 'identity' as const,
      downloadBytes: 1, downloadSha256: digest,
      installedBytes: 1, installedSha256: digest
    },
    details: {
      file: 'details.bin', encoding: 'identity' as const,
      downloadBytes: 1, downloadSha256: digest,
      installedBytes: 1, installedSha256: digest
    }
  };
  return {
    ...unsigned,
    manifestSha256: sha256(analyzerManifestDigestInput(unsigned))
  };
}

const RELEASE = release();

class LifecycleWorker extends EventTarget {
  static instances: LifecycleWorker[] = [];

  readonly requests: WorkerRequest[] = [];
  terminated = false;

  constructor() {
    super();
    LifecycleWorker.instances.push(this);
  }

  postMessage(request: WorkerRequest): void {
    this.requests.push(request);
  }

  terminate(): void {
    this.terminated = true;
  }

  respond(result: unknown): void {
    const request = this.requests.at(-1);
    if (!request) throw new Error('Worker has no request to answer');
    const response: WorkerResponse = { id: request.id, type: 'result', result };
    this.dispatchEvent(new MessageEvent('message', { data: response }));
  }

  fail(code: AnalyzerClientErrorCode, message: string): void {
    const request = this.requests.at(-1);
    if (!request) throw new Error('Worker has no request to answer');
    const response: WorkerResponse = { id: request.id, type: 'error', code, message };
    this.dispatchEvent(new MessageEvent('message', { data: response }));
  }

  crash(): Event {
    const event = new Event('error', { cancelable: true });
    this.dispatchEvent(event);
    return event;
  }
}

afterEach(() => {
  LifecycleWorker.instances = [];
});

const lifecycleWorker = (): Worker => new LifecycleWorker() as unknown as Worker;

describe('AnalyzerClient Worker lifecycle', () => {
  test('rejects an invalid deployed release before constructing a Worker', async () => {
    const client = new AnalyzerClient(lifecycleWorker);
    await expect(client.expectRelease({
      ...RELEASE,
      manifestSha256: '0'.repeat(64)
    })).rejects.toMatchObject({
      name: 'AnalyzerClientError',
      code: 'invalid-pack'
    });
    expect(LifecycleWorker.instances).toHaveLength(0);
    client.dispose();
  });

  test('surfaces synchronous boot failure and retries only on an explicit request', async () => {
    let attempts = 0;
    const client = new AnalyzerClient(() => {
      if (attempts++ === 0) throw new Error('Worker construction failed');
      return lifecycleWorker();
    });
    await expect(client.expectRelease(RELEASE)).rejects.toMatchObject({
      code: 'worker-unavailable'
    });
    const recovered = client.status();
    expect(LifecycleWorker.instances).toHaveLength(1);
    LifecycleWorker.instances[0]!.respond({ state: 'not-installed' });
    expect(await recovered).toEqual({ state: 'not-installed' });
    client.dispose();
  });

  test('recovers an interrupted install on the next status request', async () => {
    const client = new AnalyzerClient(lifecycleWorker);
    const pin = client.expectRelease(RELEASE);
    const first = LifecycleWorker.instances[0]!;
    expect(first.requests.at(-1)?.op).toBe('expect-release');
    first.respond({ state: 'not-installed' });
    await pin;

    const install = client.install('/analyzer/manifest.json', () => undefined);
    const crash = first.crash();

    await expect(install).rejects.toMatchObject<Partial<AnalyzerClientError>>({
      code: 'worker-crashed'
    });
    expect(crash.defaultPrevented).toBe(true);
    expect(first.terminated).toBe(true);
    expect(LifecycleWorker.instances).toHaveLength(1);

    const status = client.status();
    const replacement = LifecycleWorker.instances[1]!;
    expect(replacement.requests.at(-1)?.op).toBe('expect-release');
    replacement.respond({ state: 'incomplete', message: 'Install was interrupted.' });
    expect(await status).toEqual({ state: 'incomplete', message: 'Install was interrupted.' });
    client.dispose();
  });

  test('retries persistent boot failures only when another request is made', async () => {
    const client = new AnalyzerClient(lifecycleWorker);

    const firstStatus = client.expectRelease(RELEASE);
    LifecycleWorker.instances[0]!.crash();
    await expect(firstStatus).rejects.toMatchObject({ code: 'worker-crashed' });
    expect(LifecycleWorker.instances).toHaveLength(1);

    const secondStatus = client.status();
    expect(LifecycleWorker.instances[1]!.requests.at(-1)?.op).toBe('expect-release');
    LifecycleWorker.instances[1]!.crash();
    await expect(secondStatus).rejects.toMatchObject({ code: 'worker-crashed' });
    expect(LifecycleWorker.instances).toHaveLength(2);

    const recoveredStatus = client.status();
    expect(LifecycleWorker.instances[2]!.requests.at(-1)?.op).toBe('expect-release');
    LifecycleWorker.instances[2]!.respond({ state: 'not-installed' });
    expect(await recoveredStatus).toEqual({ state: 'not-installed' });
    client.dispose();
  });

  test('supersedes obsolete analysis without waiting for its Worker', async () => {
    const client = new AnalyzerClient(lifecycleWorker);
    const pin = client.expectRelease(RELEASE);
    LifecycleWorker.instances[0]!.respond({ state: 'ready' });
    await pin;
    const obsolete = client.analyze('古い', { limit: 1 });

    client.restart();
    await expect(obsolete).rejects.toMatchObject({ code: 'request-superseded' });
    expect(LifecycleWorker.instances[0]!.terminated).toBe(true);

    const current = client.analyze('新しい', { limit: 1 });
    const replacement = LifecycleWorker.instances[1]!;
    expect(replacement.requests.at(-1)?.op).toBe('expect-release');
    replacement.respond({ state: 'ready' });
    await new Promise(resolve => setTimeout(resolve, 0));
    expect(replacement.requests.at(-1)?.op).toBe('analyze');
    replacement.respond({
      input: '新しい', normalized: '新しい', computeMs: 1, paths: []
    });
    expect((await current).input).toBe('新しい');
    client.dispose();
  });

  test('pins a changed release on a fresh Worker before exposing status', async () => {
    const client = new AnalyzerClient(lifecycleWorker);
    const firstPin = client.expectRelease(RELEASE);
    LifecycleWorker.instances[0]!.respond({ state: 'not-installed' });
    await firstPin;

    const nextRelease = release('next-release');
    const nextPin = client.expectRelease(nextRelease);
    const replacement = LifecycleWorker.instances[1]!;
    expect(LifecycleWorker.instances[0]!.terminated).toBe(true);
    expect(replacement.requests).toHaveLength(1);
    expect(replacement.requests[0]).toMatchObject({
      op: 'expect-release',
      release: { manifestSha256: nextRelease.manifestSha256 }
    });
    replacement.respond({ state: 'stale', message: 'Reinstall',
      installedPackVersion: RELEASE.packVersion,
      installedManifestSha256: RELEASE.manifestSha256,
      expectedPackVersion: nextRelease.packVersion,
      expectedManifestSha256: nextRelease.manifestSha256
    });
    expect((await nextPin).state).toBe('stale');
    client.dispose();
  });

  test('does not post to a Worker superseded during release initialization', async () => {
    const client = new AnalyzerClient(lifecycleWorker);
    const firstPin = client.expectRelease(RELEASE);
    LifecycleWorker.instances[0]!.respond({ state: 'not-installed' });
    await firstPin;

    const redundantPin = client.expectRelease(RELEASE);
    client.restart();
    await expect(redundantPin).rejects.toMatchObject({ code: 'worker-crashed' });

    const status = client.status();
    const replacement = LifecycleWorker.instances[1]!;
    replacement.respond({ state: 'not-installed' });
    expect(await status).toEqual({ state: 'not-installed' });
    client.dispose();
  });

  test('uses the clean romanize and dictionary-entry protocol with structured errors', async () => {
    const client = new AnalyzerClient(lifecycleWorker);
    const pin = client.expectRelease(RELEASE);
    const worker = LifecycleWorker.instances[0]!;
    worker.respond({ state: 'ready' });
    await pin;

    const romanized = client.romanize('猫。', {
      method: 'kunrei-siki',
      normalizePunctuation: true
    });
    expect(worker.requests.at(-1)).toMatchObject({
      op: 'romanize',
      text: '猫。',
      options: { method: 'kunrei-siki', normalizePunctuation: true }
    });
    worker.respond('neko.');
    expect(await romanized).toBe('neko.');

    const entry = client.entry(42);
    expect(worker.requests.at(-1)).toEqual({ id: 3, op: 'entry', entryIndex: 42 });
    worker.fail('not-found', 'No dictionary entry at index 42');
    await expect(entry).rejects.toMatchObject({
      name: 'AnalyzerClientError',
      code: 'not-found',
      message: 'No dictionary entry at index 42'
    });
    client.dispose();
  });
});
