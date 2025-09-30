#!/usr/bin/env node

/**
 * REST API server for Ichiran
 * Exposes the CLI functionality via HTTP endpoints
 */

import { createServer, IncomingMessage, ServerResponse } from 'http';
import { romanize, romanizeStar } from './romanize.js';
import { setConnection, getConnectionFromEnv } from './conn.js';
import { wordInfoGlossJson, printPerfCounters } from './dict.js';
import type { WordInfo } from './dict.js';

const PORT = parseInt(process.env.PORT || '3000', 10);

/**
 * Transform romanizeStar result to user-friendly JSON format
 */
async function transformRomanizeStarResult(
  result: Array<string | Array<[Array<[string, WordInfo, any]>, number]>>
): Promise<any> {
  return Promise.all(
    result.map(async (segment) => {
      if (typeof segment === 'string') {
        return segment;
      } else {
        return Promise.all(
          segment.map(async ([wordList, score]) => {
            const transformedWords = await Promise.all(
              wordList.map(async ([romanized, wordInfo, prop]) => {
                const glossJson = await wordInfoGlossJson(wordInfo);
                return [romanized, glossJson, prop];
              })
            );
            return [transformedWords, score];
          })
        );
      }
    })
  );
}

/**
 * Parse JSON body from request
 */
async function parseJsonBody(req: IncomingMessage): Promise<any> {
  return new Promise((resolve, reject) => {
    let body = '';
    req.on('data', (chunk) => {
      body += chunk.toString();
    });
    req.on('end', () => {
      try {
        resolve(JSON.parse(body));
      } catch (error) {
        reject(new Error('Invalid JSON'));
      }
    });
    req.on('error', reject);
  });
}

/**
 * Send JSON response
 */
function sendJson(res: ServerResponse, data: any, status = 200, requestId?: string): void {
  const json = JSON.stringify(data);
  res.writeHead(status, { 'Content-Type': 'application/json' });
  res.end(json);
  if (requestId) {
    console.log(`[${requestId}] Response sent: ${json.length} bytes, status ${status}`);
  }
}

/**
 * Send error response
 */
function sendError(res: ServerResponse, message: string, status = 400): void {
  sendJson(res, { error: message }, status);
}

/**
 * Main request handler
 */
async function handleRequest(req: IncomingMessage, res: ServerResponse): Promise<void> {
  const requestId = Math.random().toString(36).substring(7);
  const startTime = Date.now();
  const url = new URL(req.url || '/', `http://${req.headers.host}`);

  console.log(`[${requestId}] START ${req.method} ${url.pathname}`);

  // CORS headers
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');

  // Handle OPTIONS for CORS preflight
  if (req.method === 'OPTIONS') {
    res.writeHead(204);
    res.end();
    console.log(`[${requestId}] END OPTIONS ${url.pathname} - ${Date.now() - startTime}ms`);
    return;
  }

  try {
    // Health check endpoint
    if (url.pathname === '/health' && req.method === 'GET') {
      sendJson(res, { status: 'ok', timestamp: new Date().toISOString() }, 200, requestId);
      console.log(`[${requestId}] END ${url.pathname} - ${Date.now() - startTime}ms`);
      return;
    }

    // Database health check endpoint
    if (url.pathname === '/health/db' && req.method === 'GET') {
      console.log(`[${requestId}] Testing database connection...`);
      try {
        const { getConnection } = await import('./conn.js');
        const conn = getConnection();
        console.log(`[${requestId}] Got connection, executing test query...`);
        const result = await conn`SELECT 1 as test, current_database() as db, version() as pg_version`;
        console.log(`[${requestId}] Query result:`, result);
        sendJson(res, {
          status: 'ok',
          database: 'connected',
          result: result[0],
          timestamp: new Date().toISOString()
        }, 200, requestId);
        console.log(`[${requestId}] END ${url.pathname} - ${Date.now() - startTime}ms`);
        return;
      } catch (dbError) {
        console.error(`[${requestId}] Database error:`, dbError);
        sendJson(res, {
          status: 'error',
          database: 'failed',
          error: dbError instanceof Error ? dbError.message : String(dbError),
          stack: dbError instanceof Error ? dbError.stack : undefined
        }, 500, requestId);
        console.log(`[${requestId}] END ${url.pathname} DB ERROR - ${Date.now() - startTime}ms`);
        return;
      }
    }

    // Minimal test endpoint (no database)
    if (url.pathname === '/api/test' && req.method === 'POST') {
      const body = await parseJsonBody(req);
      console.log(`[${requestId}] Test endpoint - body: ${JSON.stringify(body)}`);
      sendJson(res, {
        echo: body,
        timestamp: new Date().toISOString(),
        memory: process.memoryUsage(),
        uptime: process.uptime()
      }, 200, requestId);
      console.log(`[${requestId}] END ${url.pathname} - ${Date.now() - startTime}ms`);
      return;
    }

    // Basic romanization: POST /api/romanize
    if (url.pathname === '/api/romanize' && req.method === 'POST') {
      const body = await parseJsonBody(req);
      console.log(`[${requestId}] Body parsed: ${JSON.stringify(body).substring(0, 100)}`);
      if (!body.text) {
        sendError(res, 'Missing required field: text');
        return;
      }

      console.log(`[${requestId}] Calling romanize...`);
      const { romanized } = await romanize(body.text, { withInfo: false });
      console.log(`[${requestId}] Romanize complete, sending response...`);
      sendJson(res, { text: body.text, romanized }, 200, requestId);
      console.log(`[${requestId}] END ${url.pathname} - ${Date.now() - startTime}ms`);
      return;
    }

    // Romanization with dictionary info: POST /api/romanize/info
    if (url.pathname === '/api/romanize/info' && req.method === 'POST') {
      const body = await parseJsonBody(req);
      if (!body.text) {
        sendError(res, 'Missing required field: text');
        return;
      }

      const { romanized, info } = await romanize(body.text, { withInfo: true });
      sendJson(res, {
        text: body.text,
        romanized,
        info: info || []
      });
      return;
    }

    // Full segmentation: POST /api/segment
    if (url.pathname === '/api/segment' && req.method === 'POST') {
      const body = await parseJsonBody(req);
      if (!body.text) {
        sendError(res, 'Missing required field: text');
        return;
      }

      const limit = body.limit ?? 1;
      const result = await romanizeStar(body.text, { limit });
      const segments = await transformRomanizeStarResult(result);

      sendJson(res, {
        text: body.text,
        segments,
        limit
      });
      return;
    }

    // API documentation endpoint
    if (url.pathname === '/api' && req.method === 'GET') {
      sendJson(res, {
        name: 'Ichiran REST API',
        version: '0.1.0',
        endpoints: {
          'GET /health': 'Health check',
          'POST /api/romanize': 'Basic romanization (body: {text: string})',
          'POST /api/romanize/info': 'Romanization with dictionary info (body: {text: string})',
          'POST /api/segment': 'Full segmentation (body: {text: string, limit?: number})'
        },
        examples: {
          romanize: {
            url: '/api/romanize',
            body: { text: 'こんにちは' }
          },
          romanizeInfo: {
            url: '/api/romanize/info',
            body: { text: '今日は良い天気です' }
          },
          segment: {
            url: '/api/segment',
            body: { text: 'ご注文はうさぎですか', limit: 3 }
          }
        }
      });
      return;
    }

    // 404 - Not found
    sendError(res, 'Not found', 404);
    console.log(`[${requestId}] END ${url.pathname} - ${Date.now() - startTime}ms`);
  } catch (error) {
    console.error(`[${requestId}] Request error:`, error);
    const message = error instanceof Error ? error.message : 'Internal server error';
    sendError(res, message, 500);
    console.log(`[${requestId}] END ${url.pathname} ERROR - ${Date.now() - startTime}ms`);
  }
}

/**
 * Start the server
 */
async function main(): Promise<void> {
  // Add global error handlers
  process.on('unhandledRejection', (reason, promise) => {
    console.error('UNHANDLED REJECTION:', reason);
    console.error('Promise:', promise);
  });

  process.on('uncaughtException', (error) => {
    console.error('UNCAUGHT EXCEPTION:', error);
    console.error('Stack:', error.stack);
  });

  // Load database connection from environment
  const connSpec = getConnectionFromEnv();
  if (!connSpec) {
    console.error('ERROR: ICHIRAN_DB_URL environment variable not set');
    process.exit(2);
  }
  console.log('Setting database connection...');
  setConnection(connSpec);
  console.log('Database connection configured');

  // Create HTTP server
  const server = createServer(handleRequest);

  // Bind to 0.0.0.0 to allow external connections
  server.listen(PORT, '0.0.0.0', () => {
    console.log(`Ichiran API server listening on http://0.0.0.0:${PORT}`);
    console.log(`Health check: http://0.0.0.0:${PORT}/health`);
    console.log(`API docs: http://0.0.0.0:${PORT}/api`);
  });

  // Graceful shutdown
  process.on('SIGTERM', () => {
    console.log('SIGTERM received, shutting down gracefully...');
    server.close(() => {
      console.log('Server closed');
      printPerfCounters();
      process.exit(0);
    });
  });

  process.on('SIGINT', () => {
    console.log('\nSIGINT received, shutting down gracefully...');
    server.close(() => {
      console.log('Server closed');
      printPerfCounters();
      process.exit(0);
    });
  });
}

// Run server if this is the entry point
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    console.error(`FATAL: ${error}`);
    process.exit(2);
  });
}
