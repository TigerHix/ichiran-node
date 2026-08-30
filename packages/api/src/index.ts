#!/usr/bin/env node

import { createServer, type IncomingMessage, type ServerResponse } from 'node:http';
import { config } from 'dotenv';
import {
  openNodeRuntime,
  romanizeWithInfo,
  type AnalyzerEntityHint
} from '@ichiran/node';

config();

const MAX_JSON_BODY_SIZE = 1024 * 1024;
type Runtime = Awaited<ReturnType<typeof openNodeRuntime>>;

class JsonBodyError extends Error {
  constructor(message: string, readonly status = 400) {
    super(message);
  }
}

async function parseJsonBody(request: IncomingMessage): Promise<Record<string, unknown>> {
  return new Promise((resolve, reject) => {
    const chunks: Buffer[] = [];
    let received = 0;
    request.on('data', (chunk: Buffer) => {
      received += chunk.byteLength;
      if (received > MAX_JSON_BODY_SIZE) {
        reject(new JsonBodyError('Payload too large', 413));
        request.destroy();
      } else {
        chunks.push(chunk);
      }
    });
    request.once('error', reject);
    request.once('end', () => {
      try {
        const value: unknown = JSON.parse(Buffer.concat(chunks).toString('utf8'));
        if (typeof value !== 'object' || value === null || Array.isArray(value)) {
          throw new JsonBodyError('JSON body must be an object');
        }
        resolve(value as Record<string, unknown>);
      } catch (error) {
        reject(error instanceof JsonBodyError ? error : new JsonBodyError('Invalid JSON'));
      }
    });
  });
}

function sendJson(response: ServerResponse, value: unknown, status = 200): void {
  response.writeHead(status, { 'Content-Type': 'application/json' });
  response.end(JSON.stringify(value));
}

function text(body: Record<string, unknown>): string {
  if (typeof body.text !== 'string' || body.text.length === 0) {
    throw new JsonBodyError('Missing required field: text');
  }
  return body.text;
}

function limit(body: Record<string, unknown>): number {
  const value = body.limit ?? 1;
  if (!Number.isSafeInteger(value) || (value as number) < 1) {
    throw new JsonBodyError('limit must be a positive integer');
  }
  return value as number;
}

function entityHints(body: Record<string, unknown>): AnalyzerEntityHint[] {
  if (!Array.isArray(body.entities)) return [];
  return body.entities.map((value, index) => {
    if (typeof value !== 'object' || value === null) {
      throw new JsonBodyError(`entities[${index}] must be an object`);
    }
    const hint = value as Partial<AnalyzerEntityHint>;
    if (
      !Number.isSafeInteger(hint.start)
      || !Number.isSafeInteger(hint.end)
      || (hint.start as number) < 0
      || (hint.end as number) <= (hint.start as number)
      || (hint.boost !== undefined && typeof hint.boost !== 'number')
    ) {
      throw new JsonBodyError(`entities[${index}] is invalid`);
    }
    return hint as AnalyzerEntityHint;
  });
}

/** Analyzer-only HTTP handler. Grammar intentionally remains a separate product. */
export function createApiHandler(runtime: Runtime) {
  return async (request: IncomingMessage, response: ServerResponse): Promise<void> => {
    response.setHeader('Access-Control-Allow-Origin', '*');
    response.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
    response.setHeader('Access-Control-Allow-Headers', 'Content-Type');
    if (request.method === 'OPTIONS') {
      response.writeHead(204);
      response.end();
      return;
    }

    const url = new URL(request.url ?? '/', `http://${request.headers.host ?? 'localhost'}`);
    try {
      if (request.method === 'GET' && url.pathname === '/health') {
        sendJson(response, { status: 'ok', timestamp: new Date().toISOString() });
        return;
      }
      if (request.method === 'GET' && url.pathname === '/health/db') {
        sendJson(response, {
          status: 'ok',
          database: 'not-required',
          result: { runtime: 'packed' },
          timestamp: new Date().toISOString()
        });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/api/test') {
        sendJson(response, {
          echo: await parseJsonBody(request),
          timestamp: new Date().toISOString(),
          memory: process.memoryUsage(),
          uptime: process.uptime()
        });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/api/romanize') {
        const body = await parseJsonBody(request);
        const input = text(body);
        sendJson(response, {
          text: input,
          romanized: await runtime.romanize(input, { normalizePunctuation: false })
        });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/api/romanize/info') {
        const body = await parseJsonBody(request);
        const input = text(body);
        const result = await romanizeWithInfo(runtime, input, false);
        sendJson(response, { text: input, ...result });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/api/segment') {
        const body = await parseJsonBody(request);
        const input = text(body);
        const pathLimit = limit(body);
        const entities = entityHints(body);
        sendJson(response, {
          text: input,
          segments: await runtime.legacy(input, {
            limit: pathLimit,
            normalizePunctuation: false,
            entities
          }),
          limit: pathLimit
        });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/api/analyze') {
        const body = await parseJsonBody(request);
        const input = text(body);
        const entities = entityHints(body);
        sendJson(response, {
          segments: await runtime.legacy(input, {
            limit: limit(body),
            normalizePunctuation: false,
            entities
          }),
          grammars: {},
          grammarExcluded: true
        });
        return;
      }
      if (request.method === 'GET' && url.pathname === '/api') {
        sendJson(response, {
          name: 'Ichiran REST API',
          version: '0.1.0',
          endpoints: {
            'GET /health': 'Health check',
            'POST /api/romanize': 'Basic romanization (body: {text: string})',
            'POST /api/romanize/info': 'Romanization with dictionary info (body: {text: string})',
            'POST /api/segment': 'Full segmentation (body: {text: string, limit?: number})',
            'POST /api/analyze': 'Analyzer segmentation; grammar excluded from this build'
          },
          examples: {
            romanize: { url: '/api/romanize', body: { text: 'こんにちは' } },
            romanizeInfo: { url: '/api/romanize/info', body: { text: '今日は良い天気です' } },
            segment: { url: '/api/segment', body: { text: 'ご注文はうさぎですか', limit: 3 } },
            analyze: { url: '/api/analyze', body: { text: '私は学生です', limit: 5 } }
          }
        });
        return;
      }
      sendJson(response, { error: 'Not found' }, 404);
    } catch (error) {
      const status = error instanceof JsonBodyError ? error.status : 500;
      sendJson(response, {
        error: error instanceof Error ? error.message : String(error)
      }, status);
    }
  };
}

export async function startApi(port = Number.parseInt(process.env.PORT ?? '3000', 10)) {
  const runtime = await openNodeRuntime();
  const server = createServer(createApiHandler(runtime));
  await new Promise<void>((resolve, reject) => {
    server.once('error', reject);
    server.listen(port, '0.0.0.0', resolve);
  });
  return server;
}

async function main(): Promise<void> {
  const server = await startApi();
  const address = server.address();
  const port = typeof address === 'object' && address ? address.port : process.env.PORT ?? '3000';
  console.log(`Ichiran API listening on http://0.0.0.0:${port}`);
}

if (import.meta.url === `file://${process.argv[1]}`) {
  void main().catch(error => {
    console.error(`FATAL: ${error instanceof Error ? error.message : String(error)}`);
    process.exit(2);
  });
}
