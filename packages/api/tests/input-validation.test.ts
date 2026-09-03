import { afterAll, beforeAll, describe, expect, test } from 'bun:test';
import { createServer, type Server } from 'node:http';

import { AnalyzerError, type Analyzer } from '@ichiran/core';
import { createApiHandler } from '../src/index.js';

describe('HTTP request validation', () => {
  let server: Server;
  let base: string;

  beforeAll(async () => {
    const analyzer = {
      analyze: async () => { throw new AnalyzerError('invalid-input', 'bad analyzer options'); },
      romanize: async () => '',
      entry: async () => { throw new AnalyzerError('not-found', 'entry does not exist'); },
      dispose: () => undefined
    } as unknown as Analyzer;
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
  });

  async function post(body: string): Promise<{ status: number; value: unknown }> {
    const response = await fetch(`${base}/v1/analyze`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body
    });
    return { status: response.status, value: await response.json() };
  }

  test('uses one structured error envelope', async () => {
    expect(await post('{')).toEqual({
      status: 400,
      value: { error: { code: 'invalid-input', message: 'Invalid JSON' } }
    });
    expect(await post('{}')).toEqual({
      status: 400,
      value: { error: { code: 'invalid-input', message: 'text must be a string' } }
    });
    expect(await post(JSON.stringify({ text: '猫', options: [] }))).toEqual({
      status: 400,
      value: { error: { code: 'invalid-input', message: 'options must be an object' } }
    });
    expect(await post(JSON.stringify({ text: '猫' }))).toEqual({
      status: 400,
      value: { error: { code: 'invalid-input', message: 'bad analyzer options' } }
    });
  });

  test('returns JSON for oversized bodies and missing entries', async () => {
    const oversized = await post(JSON.stringify({ text: 'a'.repeat(1024 * 1024) }));
    expect(oversized).toEqual({
      status: 413,
      value: { error: { code: 'invalid-input', message: 'Payload too large' } }
    });
    const missing = await fetch(`${base}/v1/entries/123`);
    expect(missing.status).toBe(404);
    expect(await missing.json()).toEqual({
      error: { code: 'not-found', message: 'entry does not exist' }
    });
  });
});
