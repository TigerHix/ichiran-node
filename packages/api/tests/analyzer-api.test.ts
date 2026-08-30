import { afterAll, beforeAll, describe, expect, test } from 'bun:test';
import { createServer, type Server } from 'node:http';

import { openNodeRuntime } from '@ichiran/node';
import { createApiHandler } from '../src/index.js';

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;

describe.skipIf(!releaseDirectory)('packed analyzer API', () => {
  let server: Server;
  let base: string;

  beforeAll(async () => {
    const runtime = await openNodeRuntime(releaseDirectory!);
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
});
