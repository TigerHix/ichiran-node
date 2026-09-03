#!/usr/bin/env node

import { createServer, type IncomingMessage, type ServerResponse } from 'node:http';
import { config } from 'dotenv';
import { openAnalyzer } from '@ichiran/node';
import {
  AnalyzerError,
  type AnalyzeOptions,
  type Analyzer,
  type AnalyzerErrorCode,
  type RomanizeOptions,
  type TokenDetailsOptions
} from '@ichiran/core';

const MAX_JSON_BODY_SIZE = 1024 * 1024;

class HttpError extends Error {
  constructor(
    readonly code: AnalyzerErrorCode,
    message: string,
    readonly status: number
  ) {
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
        reject(new HttpError('invalid-input', 'Payload too large', 413));
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
          throw new HttpError('invalid-input', 'JSON body must be an object', 400);
        }
        resolve(value as Record<string, unknown>);
      } catch (error) {
        reject(error instanceof HttpError
          ? error
          : new HttpError('invalid-input', 'Invalid JSON', 400));
      }
    });
  });
}

function sendJson(response: ServerResponse, value: unknown, status = 200): void {
  response.writeHead(status, { 'Content-Type': 'application/json' });
  response.end(JSON.stringify(value));
}

function bodyText(body: Record<string, unknown>): string {
  if (typeof body.text !== 'string') {
    throw new HttpError('invalid-input', 'text must be a string', 400);
  }
  return body.text;
}

function bodyOptions<T>(body: Record<string, unknown>): T | undefined {
  if (body.options === undefined) return undefined;
  if (typeof body.options !== 'object' || body.options === null || Array.isArray(body.options)) {
    throw new HttpError('invalid-input', 'options must be an object', 400);
  }
  return body.options as T;
}

function errorResponse(error: unknown): {
  readonly status: number;
  readonly code: AnalyzerErrorCode;
  readonly message: string;
} {
  if (error instanceof HttpError) {
    return { status: error.status, code: error.code, message: error.message };
  }
  if (error instanceof AnalyzerError) {
    const status = error.code === 'invalid-input'
      ? 400
      : error.code === 'not-found'
        ? 404
        : 500;
    return { status, code: error.code, message: error.message };
  }
  return {
    status: 500,
    code: 'internal',
    message: error instanceof Error ? error.message : String(error)
  };
}

/** HTTP transport for the same Analyzer contract used by browser and Node hosts. */
export function createApiHandler(analyzer: Analyzer) {
  return async (request: IncomingMessage, response: ServerResponse): Promise<void> => {
    response.setHeader('Access-Control-Allow-Origin', '*');
    response.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
    response.setHeader('Access-Control-Allow-Headers', 'Content-Type');
    if (request.method === 'OPTIONS') {
      response.writeHead(204);
      response.end();
      return;
    }

    try {
      const url = new URL(request.url ?? '/', 'http://localhost');
      if (request.method === 'GET' && url.pathname === '/health') {
        sendJson(response, { status: 'ok' });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/v1/analyze') {
        const body = await parseJsonBody(request);
        sendJson(response, await analyzer.analyze(
          bodyText(body),
          bodyOptions<AnalyzeOptions>(body)
        ));
        return;
      }
      if (request.method === 'POST' && url.pathname === '/v1/romanize') {
        const body = await parseJsonBody(request);
        const romanized = await analyzer.romanize(
          bodyText(body),
          bodyOptions<RomanizeOptions>(body)
        );
        sendJson(response, { romanized });
        return;
      }
      if (request.method === 'POST' && url.pathname === '/v1/details') {
        const body = await parseJsonBody(request);
        const options = bodyOptions<TokenDetailsOptions>(body);
        if (!options) {
          throw new HttpError('invalid-input', 'options are required', 400);
        }
        sendJson(response, await analyzer.details(bodyText(body), options));
        return;
      }
      const entry = request.method === 'GET'
        ? /^\/v1\/entries\/(\d+)$/.exec(url.pathname)
        : null;
      if (entry) {
        const entryIndex = Number(entry[1]);
        if (!Number.isSafeInteger(entryIndex) || entryIndex < 0) {
          throw new HttpError('invalid-input', 'entryIndex must be a non-negative integer', 400);
        }
        sendJson(response, await analyzer.entry(entryIndex));
        return;
      }
      throw new HttpError('not-found', 'Route not found', 404);
    } catch (error) {
      const failure = errorResponse(error);
      sendJson(response, {
        error: { code: failure.code, message: failure.message }
      }, failure.status);
    }
  };
}

export async function startApi(port = Number.parseInt(process.env.PORT ?? '3000', 10)) {
  const analyzer = await openAnalyzer();
  const server = createServer(createApiHandler(analyzer));
  let disposed = false;
  const dispose = () => {
    if (disposed) return;
    disposed = true;
    analyzer.dispose();
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
  config({ quiet: true });
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
