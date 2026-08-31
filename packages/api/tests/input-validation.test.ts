import { afterAll, beforeAll, describe, expect, test } from 'bun:test';
import { createServer, type Server } from 'node:http';

import {
  MAX_ANALYZER_ENTITIES,
  MAX_ANALYZER_TEXT_LENGTH
} from '@ichiran/core';
import { createApiHandler } from '../src/index.js';

describe('analyzer API request bounds', () => {
  let server: Server;
  let base: string;

  beforeAll(async () => {
    // Invalid requests must be rejected before the packed runtime is touched.
    server = createServer(createApiHandler({} as never));
    await new Promise<void>((resolve, reject) => {
      server.once('error', reject);
      server.listen(0, '127.0.0.1', resolve);
    });
    const address = server.address();
    if (!address || typeof address === 'string') throw new Error('API did not bind a TCP port');
    base = `http://127.0.0.1:${address.port}`;
  });

  afterAll(async () => {
    await new Promise<void>((resolve, reject) =>
      server.close(error => error ? reject(error) : resolve())
    );
  });

  async function invalid(body: unknown): Promise<{ status: number; error: string }> {
    const response = await fetch(`${base}/api/segment`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(body)
    });
    const value = await response.json() as { error: string };
    return { status: response.status, error: value.error };
  }

  test('returns 400 for pathological path limits, text, entities, and boosts', async () => {
    expect(await invalid({ text: '猫', limit: 100_000_000 })).toMatchObject({
      status: 400,
      error: expect.stringContaining('1 to 10')
    });
    expect(await invalid({ text: '猫'.repeat(MAX_ANALYZER_TEXT_LENGTH + 1) }))
      .toMatchObject({ status: 400, error: expect.stringContaining('text must contain at most') });
    expect(await invalid({
      text: '猫',
      entities: Array.from(
        { length: MAX_ANALYZER_ENTITIES + 1 },
        () => ({ start: 0, end: 1 })
      )
    })).toMatchObject({ status: 400, error: expect.stringContaining('entities must contain') });
    expect(await invalid({ text: '猫', entities: [{ start: 0, end: 1, boost: 1_000_001 }] }))
      .toMatchObject({ status: 400, error: expect.stringContaining('boost must be finite') });
  });

  test('returns a JSON 413 response instead of resetting an oversized upload', async () => {
    const response = await fetch(`${base}/api/segment`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ text: '猫'.repeat(1024 * 1024) })
    });
    expect(response.status).toBe(413);
    expect(await response.json()).toEqual({ error: 'Payload too large' });
  });
});
