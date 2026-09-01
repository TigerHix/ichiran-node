#!/usr/bin/env node

import { createServer, type IncomingMessage, type ServerResponse } from 'node:http';
import { config } from 'dotenv';
import {
  openNodeRuntime,
  romanizeWithInfo
} from '@ichiran/node';
import {
  AnalyzerInputError,
  validatePortableAnalyzeRequest,
  type PortableAnalyzeOptions
} from '@ichiran/core';

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
    let rejected = false;
    request.on('data', (chunk: Buffer) => {
      if (rejected) return;
      received += chunk.byteLength;
      if (received > MAX_JSON_BODY_SIZE) {
        rejected = true;
        chunks.length = 0;
        reject(new JsonBodyError('Payload too large', 413));
      } else {
        chunks.push(chunk);
      }
    });
    request.once('error', reject);
    request.once('end', () => {
      if (rejected) return;
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

function analyzerRequest(
  body: Record<string, unknown>,
  includeOptions: boolean
): { readonly input: string; readonly options: PortableAnalyzeOptions } {
  if (typeof body.text !== 'string' || body.text.length === 0) {
    throw new JsonBodyError('Missing required field: text');
  }
  try {
    const validated = validatePortableAnalyzeRequest(body.text, includeOptions ? {
      limit: body.limit === undefined ? 1 : body.limit as number,
      entities: body.entities as PortableAnalyzeOptions['entities']
    } : { limit: 1 });
    return validated;
  } catch (error) {
    if (error instanceof AnalyzerInputError) throw new JsonBodyError(error.message);
    throw error;
  }
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
        const { input } = analyzerRequest(body, false);
        sendJson(response, {
          text: input,
          romanized: await runtime.romanize(input, { normalizePunctuation: false })
        });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/api/romanize/info') {
        const body = await parseJsonBody(request);
        const { input } = analyzerRequest(body, false);
        const result = await romanizeWithInfo(runtime, input, false);
        sendJson(response, { text: input, ...result });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/api/segment') {
        const body = await parseJsonBody(request);
        const { input, options } = analyzerRequest(body, true);
        sendJson(response, {
          text: input,
          segments: await runtime.legacy(input, {
            limit: options.limit,
            normalizePunctuation: false,
            entities: options.entities
          }),
          limit: options.limit
        });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/api/analyze') {
        const body = await parseJsonBody(request);
        const { input, options } = analyzerRequest(body, true);
        sendJson(response, {
          segments: await runtime.legacy(input, {
            limit: options.limit,
            normalizePunctuation: false,
            entities: options.entities
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
      let status = 500;
      if (error instanceof JsonBodyError) status = error.status;
      else if (error instanceof AnalyzerInputError) status = 400;
      sendJson(response, {
        error: error instanceof Error ? error.message : String(error)
      }, status);
    }
  };
}

export async function startApi(port = Number.parseInt(process.env.PORT ?? '3000', 10)) {
  const runtime = await openNodeRuntime();
  const server = createServer(createApiHandler(runtime));
  let disposed = false;
  const dispose = () => {
    if (disposed) return;
    disposed = true;
    runtime.dispose();
  };
  process.once('exit', dispose);
  server.once('close', () => {
    process.off('exit', dispose);
    dispose();
  });
  try {
    await new Promise<void>((resolve, reject) => {
      server.once('error', reject);
      server.listen(port, '0.0.0.0', resolve);
    });
  } catch (error) {
    process.off('exit', dispose);
    dispose();
    throw error;
  }
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
