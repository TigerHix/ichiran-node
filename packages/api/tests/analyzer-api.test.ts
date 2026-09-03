import { afterAll, beforeAll, describe, expect, test } from 'bun:test';
import { createServer, type Server } from 'node:http';

import { openAnalyzer } from '@ichiran/node';
import { createApiHandler } from '../src/index.js';

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;

describe.skipIf(!releaseDirectory)('packed analyzer HTTP API', () => {
  let server: Server;
  let analyzer: Awaited<ReturnType<typeof openAnalyzer>>;
  let base: string;

  beforeAll(async () => {
    analyzer = await openAnalyzer(releaseDirectory!);
    server = createServer(createApiHandler(analyzer));
    await new Promise<void>((resolve, reject) => {
      server.once('error', reject);
      server.listen(0, '127.0.0.1', resolve);
    });
    const address = server.address();
    if (!address || typeof address === 'string') throw new Error('API did not bind a TCP port');
    base = `http://127.0.0.1:${address.port}`;
  });

  afterAll(async () => {
    await new Promise<void>((resolve, reject) => (
      server.close(error => error ? reject(error) : resolve())
    ));
    analyzer.dispose();
  });

  async function post(path: string, body: unknown) {
    const response = await fetch(`${base}${path}`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(body)
    });
    return { response, body: await response.json() as Record<string, unknown> };
  }

  test('serves the small versioned transport contract', async () => {
    const health = await fetch(`${base}/health`);
    expect(health.status).toBe(200);
    expect(await health.json()).toEqual({ status: 'ok' });

    const romanized = await post('/v1/romanize', { text: '今日' });
    expect(romanized.response.status).toBe(200);
    expect(romanized.body).toEqual({ romanized: 'kyō' });

    const analyzed = await post('/v1/analyze', {
      text: '今日',
      options: { limit: 1 }
    });
    expect(analyzed.response.status).toBe(200);
    expect(analyzed.body.input).toBe('今日');
    expect(Array.isArray(analyzed.body.paths)).toBe(true);
    const paths = analyzed.body.paths as { tokens: { entryIndex: number | null }[] }[];
    const entryIndex = paths[0]?.tokens.find(token => token.entryIndex !== null)?.entryIndex;
    expect(entryIndex).toBeNumber();
    const entry = await fetch(`${base}/v1/entries/${entryIndex}`);
    expect(entry.status).toBe(200);
    expect(await entry.json()).toMatchObject({ seq: expect.any(Number) });
  });

  test('preserves UTF-16 input and entity offsets', async () => {
    const analyzed = await post('/v1/analyze', {
      text: '🙂猫',
      options: { limit: 1, entities: [{ start: 2, end: 3, boost: 100 }] }
    });
    expect(analyzed.response.status).toBe(200);
    expect(analyzed.body).toMatchObject({ input: '🙂猫' });
    const paths = analyzed.body.paths as {
      readonly tokens: readonly {
        readonly start: number;
        readonly end: number;
        readonly text: string;
        readonly entity: boolean;
      }[];
    }[];
    expect(paths[0]?.tokens.find(token => token.text === '猫')).toMatchObject({
      start: 2,
      end: 3,
      entity: true
    });

    const loneSurrogate = String.fromCharCode(0xd83d);
    const preserved = await post('/v1/analyze', { text: loneSurrogate });
    expect(preserved.response.status).toBe(200);
    const echoed = preserved.body.input as string;
    expect(echoed.length).toBe(1);
    expect(echoed.charCodeAt(0)).toBe(0xd83d);
  });

  test('returns structured product errors and removes historical routes', async () => {
    const invalid = await post('/v1/analyze', { text: '猫', options: { limit: 99 } });
    expect(invalid.response.status).toBe(400);
    expect(invalid.body).toMatchObject({
      error: { code: 'invalid-input', message: expect.stringContaining('1 to 10') }
    });

    const invalidMethod = await post('/v1/romanize', {
      text: '猫',
      options: { method: 42 }
    });
    expect(invalidMethod.response.status).toBe(400);
    expect(invalidMethod.body).toMatchObject({
      error: { code: 'invalid-input', message: expect.stringContaining('romanization scheme') }
    });

    const removed = await post('/api/segment', { text: '今日' });
    expect(removed.response.status).toBe(404);
    expect(removed.body).toEqual({
      error: { code: 'not-found', message: 'Route not found' }
    });
  });
});
