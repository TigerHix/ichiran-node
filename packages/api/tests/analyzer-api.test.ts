import { afterAll, beforeAll, describe, expect, test } from 'bun:test';
import { readFile } from 'node:fs/promises';
import { createServer, type Server } from 'node:http';
import { join } from 'node:path';
import { gunzipSync } from 'node:zlib';

import {
  IchiranRuntime,
  MAX_ANALYZER_WORD_LENGTH,
  RUST_KERNEL_WASM_URL,
  type AnalyzerReleaseManifest
} from '@ichiran/core';
import { openNodeRuntime } from '@ichiran/node';
import { createApiHandler } from '../src/index.js';

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;
type Runtime = Awaited<ReturnType<typeof openNodeRuntime>>;

describe.skipIf(!releaseDirectory)('packed analyzer API', () => {
  let server: Server;
  let runtime: Runtime;
  let base: string;

  beforeAll(async () => {
    runtime = await openNodeRuntime(releaseDirectory!);
    server = createServer(createApiHandler(runtime));
    await new Promise<void>((resolve, reject) => {
      server.once('error', reject);
      server.listen(0, '127.0.0.1', resolve);
    });
    const address = server.address();
    if (!address || typeof address === 'string') throw new Error('API did not bind a TCP port');
    base = `http://127.0.0.1:${address.port}`;
  });

  afterAll(async () => {
    await new Promise<void>((resolve, reject) => server.close(error => error ? reject(error) : resolve()));
    runtime.dispose();
  });

  async function post(path: string, body: unknown) {
    const response = await fetch(`${base}${path}`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(body)
    });
    expect(response.status).toBe(200);
    return response.json() as Promise<Record<string, unknown>>;
  }

  test('keeps analyzer endpoint response shapes without a database', async () => {
    const health = await fetch(`${base}/health/db`).then(response => response.json());
    expect(health).toMatchObject({ status: 'ok', database: 'not-required' });

    expect(await post('/api/romanize', { text: '今日' })).toEqual({
      text: '今日',
      romanized: 'kyō'
    });
    expect(await post('/api/romanize/info', { text: '今日' })).toMatchObject({
      text: '今日',
      romanized: 'kyō',
      info: [['kyō', expect.stringContaining('today; this day')]]
    });
    expect(await post('/api/segment', { text: '今日', limit: 1 })).toMatchObject({
      text: '今日',
      limit: 1,
      segments: expect.any(Array)
    });
  });

  test('marks grammar as deliberately excluded while retaining analyze shape', async () => {
    expect(await post('/api/analyze', { text: '今日', limit: 1 })).toMatchObject({
      segments: expect.any(Array),
      grammars: {},
      grammarExcluded: true
    });
  });

  test('returns 400 for Rust-owned analyzable-word validation', async () => {
    const response = await fetch(`${base}/api/segment`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ text: '猫'.repeat(MAX_ANALYZER_WORD_LENGTH + 1), limit: 1 })
    });
    expect(response.status).toBe(400);
    expect(await response.json()).toEqual({
      error: `each analyzable word must contain at most ${MAX_ANALYZER_WORD_LENGTH} UTF-16 code units`
    });
  });

  test('serves overlapping detail-backed analyses independently', async () => {
    const inputs = ['食べさせられました', '三個'] as const;
    const sequential = [];
    for (const text of inputs) sequential.push(await post('/api/segment', { text, limit: 1 }));

    const manifest = JSON.parse(
      await readFile(join(releaseDirectory!, 'manifest.json'), 'utf8')
    ) as AnalyzerReleaseManifest;
    const [downloadedHot, downloadedDetails, wasm] = await Promise.all([
      readFile(join(releaseDirectory!, manifest.hot.file)),
      readFile(join(releaseDirectory!, manifest.details.file)),
      readFile(RUST_KERNEL_WASM_URL)
    ]);
    const hot = manifest.hot.encoding === 'gzip'
      ? new Uint8Array(gunzipSync(downloadedHot))
      : new Uint8Array(downloadedHot);
    const details = manifest.details.encoding === 'gzip'
      ? new Uint8Array(gunzipSync(downloadedDetails))
      : new Uint8Array(downloadedDetails);
    const reads: Array<readonly [number, number]> = [];
    let armed = false;
    let releaseFirstReads: () => void = () => undefined;
    const firstReads = new Promise<void>(resolve => {
      releaseFirstReads = resolve;
    });
    const concurrentRuntime = await IchiranRuntime.open({
      hot,
      wasm: new Uint8Array(wasm),
      details: {
        byteLength: details.byteLength,
        async read(offset, byteLength) {
          if (armed) {
            reads.push([offset, byteLength]);
            if (reads.length === inputs.length) releaseFirstReads();
            if (reads.length <= inputs.length) await firstReads;
          }
          return details.slice(offset, offset + byteLength);
        }
      }
    });
    const concurrentServer = createServer(createApiHandler(concurrentRuntime));
    try {
      await new Promise<void>((resolve, reject) => {
        concurrentServer.once('error', reject);
        concurrentServer.listen(0, '127.0.0.1', resolve);
      });
      const address = concurrentServer.address();
      if (!address || typeof address === 'string') throw new Error('API did not bind a TCP port');
      const concurrentBase = `http://127.0.0.1:${address.port}`;
      armed = true;
      const concurrent = await Promise.all(inputs.map(async text => {
        const response = await fetch(`${concurrentBase}/api/segment`, {
          method: 'POST',
          headers: { 'content-type': 'application/json' },
          body: JSON.stringify({ text, limit: 1 })
        });
        expect(response.status).toBe(200);
        return response.json() as Promise<Record<string, unknown>>;
      }));

      expect(concurrent).toEqual(sequential);
      expect(concurrent.map(result => result.text)).toEqual(inputs);
      expect(reads).toHaveLength(inputs.length);
      expect(reads[0]).not.toEqual(reads[1]);
    } finally {
      await new Promise<void>((resolve, reject) => (
        concurrentServer.close(error => error ? reject(error) : resolve())
      ));
      concurrentRuntime.dispose();
    }
  });
});
