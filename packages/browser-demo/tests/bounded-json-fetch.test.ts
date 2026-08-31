import { afterEach, describe, expect, test } from 'bun:test';
import { fetchBoundedJson } from '../src/bounded-json-fetch.js';

const originalFetch = globalThis.fetch;

afterEach(() => {
  globalThis.fetch = originalFetch;
});

describe('bounded manifest JSON fetch', () => {
  test('aborts stalled response headers', async () => {
    let aborted = false;
    globalThis.fetch = ((_input, init) => new Promise<Response>((_resolve, reject) => {
      init?.signal?.addEventListener('abort', () => {
        aborted = true;
        reject(new Error('fetch aborted'));
      }, { once: true });
    })) as typeof fetch;

    await expect(fetchBoundedJson('/manifest.json', {}, 'Test manifest', 10))
      .rejects.toThrow('Test manifest received no data');
    expect(aborted).toBe(true);
  });

  test('aborts a stalled response body', async () => {
    let aborted = false;
    globalThis.fetch = ((_input, init) => {
      const body = new ReadableStream<Uint8Array>({
        start(controller) {
          init?.signal?.addEventListener('abort', () => {
            aborted = true;
            controller.error(new Error('body aborted'));
          }, { once: true });
        }
      });
      return Promise.resolve(new Response(body, {
        headers: { 'content-type': 'application/json' }
      }));
    }) as typeof fetch;

    await expect(fetchBoundedJson('/manifest.json', {}, 'Test manifest', 10))
      .rejects.toThrow('Test manifest received no data');
    expect(aborted).toBe(true);
  });

  test('cancels a response that exceeds the manifest byte bound', async () => {
    let cancelled = false;
    globalThis.fetch = (() => Promise.resolve(new Response(
      new ReadableStream<Uint8Array>({
        start(controller) {
          controller.enqueue(new Uint8Array(64 * 1024 + 1));
        },
        cancel() {
          cancelled = true;
        }
      }),
      { headers: { 'content-type': 'application/json' } }
    ))) as typeof fetch;

    await expect(fetchBoundedJson('/manifest.json', {}, 'Test manifest', 1_000))
      .rejects.toThrow('Test manifest exceeds 65536 bytes');
    expect(cancelled).toBe(true);
  });
});
