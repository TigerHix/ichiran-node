#!/usr/bin/env node

/**
 * REST API server for Ichiran
 * Exposes the CLI functionality via HTTP endpoints
 */

import { createServer, IncomingMessage, ServerResponse } from 'http';
import { romanize, romanizeStar, setConnection, getConnection, type ConnectionSpec, printPerfCountersAndReset, transformRomanizeStarResult } from '@ichiran/core';
import { analyzeText, grammarCatalog } from '@ichiran/grammar';
import { config } from 'dotenv';

// Parse environment variables
config();

// Helper to parse connection from env (moved from core)
function getConnectionFromEnv(): ConnectionSpec | null {
  const dbUrl = process.env.ICHIRAN_DB_URL;
  if (!dbUrl) return null;

  try {
    const normalized = dbUrl.replace(/^postgresql:\/\//, 'postgres://');
    const url = new URL(normalized);

    const database = decodeURIComponent(url.pathname.replace(/^\//, ''));
    if (!database) {
      throw new Error('Database name missing');
    }

    const hostParam = url.searchParams.get('host');
    let host = url.hostname;
    if (!host && hostParam) {
      host = decodeURIComponent(hostParam);
    }
    if (!host) {
      host = 'localhost';
    }

    const portParam = url.port || url.searchParams.get('port') || undefined;
    const user = url.username ? decodeURIComponent(url.username) : '';
    const password = url.password ? decodeURIComponent(url.password) : '';

    const spec: ConnectionSpec = {
      user,
      password,
      host,
      database
    };

    if (portParam) {
      const parsedPort = Number(portParam);
      if (!Number.isFinite(parsedPort)) {
        throw new Error(`Invalid port: ${portParam}`);
      }
      spec.port = parsedPort;
    }

    const sslParam = url.searchParams.get('ssl');
    const sslMode = url.searchParams.get('sslmode');
    if (sslParam) {
      const normalizedSsl = sslParam.toLowerCase();
      if (['true', '1', 'require'].includes(normalizedSsl)) {
        spec.ssl = true;
      } else if (['false', '0', 'disable'].includes(normalizedSsl)) {
        spec.ssl = false;
      }
    } else if (sslMode) {
      const normalizedSslmode = sslMode.toLowerCase();
      if (['require', 'verify-ca', 'verify-full'].includes(normalizedSslmode)) {
        spec.ssl = true;
      } else if (normalizedSslmode === 'disable') {
        spec.ssl = false;
      }
    }

    return spec;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`Invalid database URL (${dbUrl}): ${message}`);
  }
}

const PORT = parseInt(process.env.PORT || '3000', 10);
const MAX_JSON_BODY_SIZE = 1 * 1024 * 1024; // 1 MiB

class JsonBodyError extends Error {
  status: number;

  constructor(message: string, status = 400) {
    super(message);
    this.name = 'JsonBodyError';
    this.status = status;
  }
}

/**
 * Parse JSON body from request
 */
async function parseJsonBody(req: IncomingMessage): Promise<any> {
  return new Promise((resolve, reject) => {
    let body = '';
    let received = 0;

    const contentLengthHeader = req.headers['content-length'];
    if (contentLengthHeader) {
      const contentLength = Number(contentLengthHeader);
      if (Number.isFinite(contentLength) && contentLength > MAX_JSON_BODY_SIZE) {
        reject(new JsonBodyError('Payload too large', 413));
        return;
      }
    }

    const abort = (error: JsonBodyError) => {
      req.destroy();
      reject(error);
    };

    req.on('data', (chunk) => {
      received += chunk.length;
      if (received > MAX_JSON_BODY_SIZE) {
        abort(new JsonBodyError('Payload too large', 413));
        return;
      }
      body += chunk.toString();
    });

    req.on('end', () => {
      if (!body) {
        reject(new JsonBodyError('Empty body'));
        return;
      }
      try {
        resolve(JSON.parse(body));
      } catch (error) {
        reject(new JsonBodyError('Invalid JSON'));
      }
    });

    req.on('error', (err) => {
      reject(err instanceof JsonBodyError ? err : new JsonBodyError(String(err), 400));
    });
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
      const { romanized } = await romanize(body.text, { withInfo: false, normalizePunctuation: false });
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

      const { romanized, info } = await romanize(body.text, { withInfo: true, normalizePunctuation: false });
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
      const result = await romanizeStar(body.text, { limit, normalizePunctuation: false });
      const segments = await transformRomanizeStarResult(result);

      sendJson(res, {
        text: body.text,
        segments,
        limit
      });
      return;
    }

    // Combined grammar analysis and segmentation: POST /api/analyze
    if (url.pathname === '/api/analyze' && req.method === 'POST') {
      const body = await parseJsonBody(req);
      if (!body.text) {
        sendError(res, 'Missing required field: text');
        return;
      }

      const limit = body.limit ?? 1;
      const maxMatches = body.maxMatches;
      
      // Perform combined analysis (single DB call)
      const analysis = await analyzeText(
        body.text,
        grammarCatalog,
        {
          maxMatches,
          limit,
          normalizePunctuation: false  // Preserve original punctuation
        }
      );

      // Group matches by grammarId
      const grammars: Record<string, any> = {};
      for (const match of analysis.grammarMatches) {
        if (!grammars[match.grammarId]) {
          grammars[match.grammarId] = {
            matchedSentences: [],
            grammarDetail: analysis.grammarDetails[match.grammarId] || {}
          };
        }
        grammars[match.grammarId].matchedSentences.push({
          level: match.level,
          description: match.description,
          captures: match.captures,
          segments: match.segments
        });
      }

      sendJson(res, {
        segments: analysis.segments,
        grammars
      });

      printPerfCountersAndReset();
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
          'POST /api/segment': 'Full segmentation (body: {text: string, limit?: number})',
          'POST /api/analyze': 'Combined grammar analysis and segmentation (body: {text: string, limit?: number, maxMatches?: number})'
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
          },
          analyze: {
            url: '/api/analyze',
            body: { text: '私は学生です', limit: 5 },
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
    if (error instanceof JsonBodyError) {
      sendError(res, error.message, error.status);
    } else {
      const message = error instanceof Error ? error.message : 'Internal server error';
      sendError(res, message, 500);
    }
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
      printPerfCountersAndReset();
      process.exit(0);
    });
  });

  process.on('SIGINT', () => {
    console.log('\nSIGINT received, shutting down gracefully...');
    server.close(() => {
      console.log('Server closed');
      printPerfCountersAndReset();
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
