#!/usr/bin/env node

/**
 * REST API server for Ichiran
 * Exposes the CLI functionality via HTTP endpoints
 */

import { createServer, IncomingMessage, ServerResponse } from 'http';
import { romanize, romanizeStar, setConnection, getConnection, type ConnectionSpec, printPerfCountersAndReset, transformRomanizeStarResult, type RomanizeStarResult, type EntityHint } from '@ichiran/core';
import { GrammarEngine, BUNPRO_RULESETS, type MatchHit } from '@ichiran/grammar';
import { config } from 'dotenv';
import { LRUCache } from 'lru-cache';
import { preprocessText, isLLMEnabled, getLLMConfig } from './llm-preprocess.js';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { existsSync } from 'fs';

let grammarEngine: GrammarEngine | null = null;

// =============================================================================
// SENTENCE-LEVEL MEMOIZATION FOR romanizeStar
// =============================================================================

// Terminal punctuation that splits sentences
const SENTENCE_SPLIT_REGEX = /([。？！?!]+)/;

interface SentenceCacheValue {
  result: RomanizeStarResult;
}

// LRU cache for sentence-level romanizeStar results
const sentenceCache = new LRUCache<string, SentenceCacheValue>({
  max: 5000,
  // Cache entries are relatively small (just result references)
});

function makeSentenceCacheKey(text: string, limit: number): string {
  return JSON.stringify({ text, limit });
}

interface SentenceSegment {
  type: 'sentence' | 'punctuation';
  text: string;
  start: number;
  end: number;
}

/**
 * Split text into sentences and punctuation segments
 * Preserves original positions for entity hint adjustment
 */
function splitIntoSentences(text: string): SentenceSegment[] {
  const segments: SentenceSegment[] = [];
  const parts = text.split(SENTENCE_SPLIT_REGEX);
  let offset = 0;

  for (let i = 0; i < parts.length; i++) {
    const part = parts[i];
    if (part.length === 0) continue;

    const isPunctuation = i % 2 === 1; // Split regex captures are at odd indices
    segments.push({
      type: isPunctuation ? 'punctuation' : 'sentence',
      text: part,
      start: offset,
      end: offset + part.length
    });
    offset += part.length;
  }

  return segments;
}

/**
 * Process romanizeStar with sentence-level caching and parallelization
 */
async function romanizeStarWithCache(
  text: string,
  options: { limit?: number; entities?: EntityHint[] } = {}
): Promise<RomanizeStarResult> {
  const limit = options.limit ?? 1;
  const entities = options.entities ?? [];

  // Split into sentences
  const segments = splitIntoSentences(text);

  // If no splitting occurred (single sentence), just process directly
  if (segments.length <= 1) {
    const cacheKey = makeSentenceCacheKey(text, limit);
    const cached = sentenceCache.get(cacheKey);
    if (cached) return cached.result;

    const result = await romanizeStar(text, { limit, normalizePunctuation: false, entities });
    sentenceCache.set(cacheKey, { result });
    return result;
  }

  // Process each segment - parallelize sentence segments, punctuation is trivial
  const segmentResults = await Promise.all(
    segments.map(async (seg): Promise<RomanizeStarResult> => {
      if (seg.type === 'punctuation') {
        // Punctuation segments are returned as-is (string in result array)
        return [seg.text];
      }

      // Check cache for this sentence
      const cacheKey = makeSentenceCacheKey(seg.text, limit);
      const cached = sentenceCache.get(cacheKey);
      if (cached) return cached.result;

      // Filter entities for this segment's range and adjust offsets
      const segmentEntities = entities
        .filter(e => e.start >= seg.start && e.end <= seg.end)
        .map(e => ({
          start: e.start - seg.start,
          end: e.end - seg.start,
          boost: e.boost
        }));

      // Process sentence
      const result = await romanizeStar(seg.text, {
        limit,
        normalizePunctuation: false,
        entities: segmentEntities
      });

      // Cache result
      sentenceCache.set(cacheKey, { result });
      return result;
    })
  );

  // Flatten results - each segment result is an array, concatenate them
  return segmentResults.flat();
}

// Parse environment variables - search multiple locations
function loadEnv() {
  // Try cwd first
  if (existsSync(resolve(process.cwd(), '.env'))) {
    config();
    console.log(`Loaded .env from ${process.cwd()}`);
    return;
  }

  // Try monorepo root (3 levels up from packages/api/src)
  const __dirname = dirname(fileURLToPath(import.meta.url));
  const monorepoRoot = resolve(__dirname, '../../..');
  const rootEnv = resolve(monorepoRoot, '.env');
  if (existsSync(rootEnv)) {
    config({ path: rootEnv });
    console.log(`Loaded .env from ${monorepoRoot}`);
    return;
  }

  console.log('No .env file found, using existing environment variables');
}
loadEnv();

// Helper to parse connection from env (moved from core)
function getConnectionFromEnv(): ConnectionSpec | null {
  const dbUrl = process.env.ICHIRAN_DB_URL || 'postgresql://postgres:password@localhost:6777/jmdict';
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
      let textToProcess = body.text;
      let entities: EntityHint[] = body.entities ?? [];

      // LLM preprocessing: normalize text and extract named entities
      let llmStats: Record<string, any> | undefined;
      let properNouns: string[] = [];
      if (isLLMEnabled() && !body.skipLLM) {
        const preprocessed = await preprocessText(body.text);
        textToProcess = preprocessed.text;
        properNouns = preprocessed.properNouns;
        entities = [...entities, ...preprocessed.entities];
        if (preprocessed.stats) {
          llmStats = {
            latencyMs: preprocessed.stats.latencyMs,
            tokens: preprocessed.stats.tokens,
            model: preprocessed.stats.model,
            cached: preprocessed.stats.cached,
            retries: preprocessed.stats.retries
          };
        }
        console.log(`[${requestId}] LLM preprocess: ${preprocessed.stats?.latencyMs ?? 0}ms, ${preprocessed.stats?.cached ? 'cached' : 'fresh'}, entities=${preprocessed.entities.length}`);
      }

      const segmentStart = performance.now();
      const result = await romanizeStarWithCache(textToProcess, { limit, entities });
      const segments = await transformRomanizeStarResult(result);
      const segmentMs = Math.round(performance.now() - segmentStart);

      console.log(`[${requestId}] segment=${segmentMs}ms`);

      const response: Record<string, any> = {
        text: textToProcess,
        segments,
        limit
      };

      if (isLLMEnabled() && !body.skipLLM) {
        if (textToProcess !== body.text) {
          response.normalizedText = textToProcess;
          response.originalText = body.text;
        }
        if (properNouns.length > 0) {
          response.properNouns = properNouns;
        }
        if (llmStats) {
          response.llm = llmStats;
        }
      }

      response.timing = {
        segmentMs,
        ...(llmStats?.latencyMs !== undefined && { llmMs: llmStats.latencyMs })
      };

      sendJson(res, response);
      return;
    }

    // Combined grammar analysis and segmentation: POST /api/analyze
    if (url.pathname === '/api/analyze' && req.method === 'POST') {
      const body = await parseJsonBody(req);
      if (!body.text) {
        sendError(res, 'Missing required field: text');
        return;
      }

      if (!grammarEngine) {
        sendError(res, 'Grammar engine not initialized', 500);
        return;
      }

      const limit = body.limit ?? 1;
      let textToProcess = body.text;
      let entities: EntityHint[] = body.entities ?? [];

      // LLM preprocessing: normalize text and extract named entities
      let llmStats: Record<string, any> | undefined;
      let properNouns: string[] = [];
      if (isLLMEnabled() && !body.skipLLM) {
        const preprocessed = await preprocessText(body.text);
        textToProcess = preprocessed.text;
        properNouns = preprocessed.properNouns;
        // Merge LLM-extracted entities with any provided entities
        entities = [...entities, ...preprocessed.entities];
        if (preprocessed.stats) {
          llmStats = {
            latencyMs: preprocessed.stats.latencyMs,
            tokens: preprocessed.stats.tokens,
            model: preprocessed.stats.model,
            cached: preprocessed.stats.cached,
            retries: preprocessed.stats.retries
          };
        }
        console.log(`[${requestId}] LLM preprocess: ${preprocessed.stats?.latencyMs ?? 0}ms, ${preprocessed.stats?.cached ? 'cached' : 'fresh'}, entities=${preprocessed.entities.length}, properNouns=${properNouns.join(',') || 'none'}`);
      }

      const segmentStart = performance.now();
      // Get segmentation with sentence-level caching
      const result = await romanizeStarWithCache(textToProcess, { limit, entities });
      const segments = await transformRomanizeStarResult(result);
      const segmentMs = Math.round(performance.now() - segmentStart);

      const grammarStart = performance.now();
      // Get grammar matches (on normalized text if LLM was used)
      const matches: MatchHit[] = await grammarEngine.match(textToProcess, {
        rulesetIds: body.rulesetIds
      });
      const grammarMs = Math.round(performance.now() - grammarStart);

      console.log(`[${requestId}] segment=${segmentMs}ms, grammar=${grammarMs}ms`);

      // Group matches by ruleId (details fetched separately via /api/grammar/:id)
      const grammars: Record<string, any> = {};
      for (const match of matches) {
        if (!grammars[match.ruleId]) {
          grammars[match.ruleId] = {
            rulesetId: match.rulesetId,
            matches: []
          };
        }
        grammars[match.ruleId].matches.push({
          captures: match.captures
        });
      }

      const response: Record<string, any> = {
        segments,
        grammars
      };

      // Include normalized text and LLM stats in response if LLM was used
      if (isLLMEnabled() && !body.skipLLM) {
        if (textToProcess !== body.text) {
          response.normalizedText = textToProcess;
          response.originalText = body.text;
        }
        if (properNouns.length > 0) {
          response.properNouns = properNouns;
        }
        if (llmStats) {
          response.llm = llmStats;
        }
      }

      // Include timing in response
      response.timing = {
        segmentMs,
        grammarMs,
        ...(llmStats?.latencyMs !== undefined && { llmMs: llmStats.latencyMs })
      };

      sendJson(res, response);

      printPerfCountersAndReset();
      return;
    }

    // Grammar rule details endpoint
    const grammarMatch = url.pathname.match(/^\/api\/grammar\/(.+)$/);
    if (grammarMatch && req.method === 'GET') {
      if (!grammarEngine) {
        sendError(res, 'Grammar engine not initialized', 500);
        return;
      }
      const ruleId = decodeURIComponent(grammarMatch[1]!);
      const details = grammarEngine.getRuleDetails(ruleId);
      if (!details) {
        sendError(res, `Rule not found: ${ruleId}`, 404);
        return;
      }
      sendJson(res, details);
      return;
    }

    // API documentation endpoint
    if (url.pathname === '/api' && req.method === 'GET') {
      const llmConfig = getLLMConfig();
      sendJson(res, {
        name: 'Ichiran REST API',
        version: '0.1.0',
        llm: {
          enabled: isLLMEnabled(),
          model: llmConfig?.model ?? null,
          description: 'When USE_LLM=true, /segment and /analyze normalize text and extract named entities via LLM'
        },
        endpoints: {
          'GET /health': 'Health check',
          'POST /api/romanize': 'Basic romanization (body: {text: string})',
          'POST /api/romanize/info': 'Romanization with dictionary info (body: {text: string})',
          'POST /api/segment': 'Full segmentation (body: {text: string, limit?: number, skipLLM?: boolean})',
          'POST /api/analyze': 'Combined grammar analysis and segmentation (body: {text: string, limit?: number, rulesetIds?: string[], skipLLM?: boolean})',
          'GET /api/grammar/:ruleId': 'Get grammar rule details (returns: {ruleId, rulesetId, name?, description?})'
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

  // Initialize grammar engine
  console.log('Initializing grammar engine...');
  grammarEngine = await GrammarEngine.create(BUNPRO_RULESETS);
  console.log(`Grammar engine ready with ${grammarEngine.getRuleIds().length} rules`);

  // Create HTTP server
  const server = createServer(handleRequest);

  // Bind to 0.0.0.0 to allow external connections
  server.listen(PORT, '0.0.0.0', () => {
    console.log(`Ichiran API server listening on http://0.0.0.0:${PORT}`);
    console.log(`Health check: http://0.0.0.0:${PORT}/health`);
    console.log(`API docs: http://0.0.0.0:${PORT}/api`);
    if (isLLMEnabled()) {
      const config = getLLMConfig();
      console.log(`LLM preprocessing: enabled (model: ${config?.model ?? 'not configured'})`);
    }
  });

  // Graceful shutdown
  const shutdown = async () => {
    server.close(async () => {
      console.log('Server closed');
      if (grammarEngine) {
        await grammarEngine.close();
        console.log('Grammar engine closed');
      }
      printPerfCountersAndReset();
      process.exit(0);
    });
  };

  process.on('SIGTERM', () => {
    console.log('SIGTERM received, shutting down gracefully...');
    shutdown();
  });

  process.on('SIGINT', () => {
    console.log('\nSIGINT received, shutting down gracefully...');
    shutdown();
  });
}

// Run server if this is the entry point
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    console.error(`FATAL: ${error}`);
    process.exit(2);
  });
}
