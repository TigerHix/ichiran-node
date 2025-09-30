// ichiran/conn - Database connection management

import postgres from 'postgres';
import fs from 'fs';
import path from 'path';

export interface ConnectionSpec {
  database: string;
  user: string;
  password: string;
  host: string;
  port?: number;
  ssl?: boolean;
}

let connection: postgres.Sql | null = null;
let connectionSpec: ConnectionSpec | null = null;

// INSTRUMENTATION: Track query count and log queries
let queryCount = 0;
let queryLog: Array<{query: string; params: any[]; stack?: string; timestamp: number}> = [];
let logQueries = false;
let queryLogFile: string | null = null;
let queryLogStream: fs.WriteStream | null = null;

export function resetQueryCount() {
  queryCount = 0;
  queryLog = [];
}
export function getQueryCount() { return queryCount; }
export function enableQueryLogging(logFilePath?: string) {
  logQueries = true;
  queryLog = [];

  if (logFilePath) {
    // Ensure logs directory exists
    const logsDir = path.dirname(logFilePath);
    if (!fs.existsSync(logsDir)) {
      fs.mkdirSync(logsDir, { recursive: true });
    }

    queryLogFile = logFilePath;
    queryLogStream = fs.createWriteStream(logFilePath, { flags: 'w' });
    queryLogStream.write('=== DATABASE QUERY LOG ===\n');
    queryLogStream.write(`Started: ${new Date().toISOString()}\n\n`);
  }
}
export function disableQueryLogging() {
  logQueries = false;
  if (queryLogStream) {
    queryLogStream.end();
    queryLogStream = null;
  }
  queryLogFile = null;
}
export function getQueryLog() { return queryLog; }
export function getQuerySummary() {
  const patterns = new Map<string, number>();

  for (const log of queryLog) {
    // Extract table name and operation
    const query = log.query.toLowerCase();
    let pattern = 'unknown';

    if (query.includes('select') && query.includes('from entry')) {
      pattern = 'SELECT FROM entry';
    } else if (query.includes('select') && query.includes('from sense_prop')) {
      pattern = 'SELECT FROM sense_prop';
    } else if (query.includes('select') && query.includes('from sense')) {
      pattern = 'SELECT FROM sense';
    } else if (query.includes('select') && query.includes('from conjugation')) {
      pattern = 'SELECT FROM conjugation';
    } else if (query.includes('select') && query.includes('from kanji_text')) {
      pattern = 'SELECT FROM kanji_text';
    } else if (query.includes('select') && query.includes('from kana_text')) {
      pattern = 'SELECT FROM kana_text';
    } else if (query.includes('select') && query.includes('from gloss')) {
      pattern = 'SELECT FROM gloss';
    } else {
      // Extract first 50 chars
      pattern = query.slice(0, 50);
    }

    patterns.set(pattern, (patterns.get(pattern) || 0) + 1);
  }

  return Array.from(patterns.entries())
    .sort((a, b) => b[1] - a[1]);
}

export function getConnectionFromEnv(): ConnectionSpec | null {
  const dbUrl = process.env.ICHIRAN_TEST_DB_URL || process.env.ICHIRAN_DB_URL;
  if (!dbUrl) return null;

  // Parse PostgreSQL connection URL
  const match = dbUrl.match(/postgresql:\/\/([^:]+):([^@]+)@([^:]+):(\d+)\/(.+)/);
  if (!match) {
    throw new Error(`Invalid database URL: ${dbUrl}`);
  }

  return {
    user: match[1],
    password: match[2],
    host: match[3],
    port: parseInt(match[4]),
    database: match[5]
  };
}

export function setConnection(spec: ConnectionSpec) {
  if (connection) {
    connection.end();
  }

  connectionSpec = spec;
  const rawConnection = postgres({
    host: spec.host,
    port: spec.port ?? 5432,
    database: spec.database,
    user: spec.user,
    password: spec.password,
    ssl: spec.ssl ? 'require' : false,
    // Automatically convert snake_case ↔ camelCase for column names only
    transform: postgres.camel
  });

  // INSTRUMENTATION: Wrap connection to count queries
  connection = new Proxy(rawConnection, {
    apply(target, thisArg, argsList) {
      queryCount++;

      // Log query if enabled
      if (logQueries && argsList.length > 0) {
        const [strings, ...params] = argsList;
        if (strings && strings.raw) {
          const queryStr = strings.raw.join('?');

          // Capture stack trace
          const stack = new Error().stack || '';
          const stackLines = stack.split('\n').slice(3, 8); // Skip proxy frames, take 5 relevant frames
          const callerInfo = stackLines.join('\n    ');

          const logEntry = {
            query: queryStr,
            params: params.map(p => {
              if (Array.isArray(p)) return `[${p.length} items]`;
              if (typeof p === 'object') return JSON.stringify(p);
              return String(p);
            }),
            stack: callerInfo,
            timestamp: Date.now()
          };

          queryLog.push(logEntry);

          // Write to file if stream is open
          if (queryLogStream) {
            queryLogStream.write(`\n${'='.repeat(80)}\n`);
            queryLogStream.write(`Query #${queryCount}\n`);
            queryLogStream.write(`Query: ${queryStr}\n`);
            if (params.length > 0) {
              queryLogStream.write(`Params: ${logEntry.params.join(', ')}\n`);
            }
            queryLogStream.write(`\nCalled from:\n    ${callerInfo}\n`);
          }
        }
      }

      return Reflect.apply(target, thisArg, argsList);
    }
  }) as any;
}

export function getConnection(): postgres.Sql {
  if (!connection) {
    const spec = getConnectionFromEnv();
    if (!spec) {
      throw new Error('No database connection configured. Set ICHIRAN_TEST_DB_URL or ICHIRAN_DB_URL environment variable.');
    }
    setConnection(spec);
  }
  return connection!;
}

export async function withDb<T>(fn: (sql: postgres.Sql) => Promise<T>): Promise<T> {
  const sql = getConnection();
  return await fn(sql);
}

// Cache infrastructure
interface Cache<T> {
  data: T | null;
  initialized: boolean;
  init: () => Promise<T>;
}

const caches = new Map<string, Cache<any>>();

export function defineCache<T>(
  name: string,
  initFn: () => Promise<T>
): () => Promise<T> {
  const cache: Cache<T> = {
    data: null,
    initialized: false,
    init: initFn
  };

  caches.set(name, cache);

  return async () => {
    if (!cache.initialized) {
      cache.data = await cache.init();
      cache.initialized = true;
    }
    return cache.data!;
  };
}

export async function initAllCaches(force = false): Promise<void> {
  for (const cache of caches.values()) {
    if (force || !cache.initialized) {
      cache.data = await cache.init();
      cache.initialized = true;
    }
  }
}

export function resetCache(name: string): void {
  const cache = caches.get(name);
  if (cache) {
    cache.initialized = false;
    cache.data = null;
  }
}

export function resetAllCaches(): void {
  for (const cache of caches.values()) {
    cache.initialized = false;
    cache.data = null;
  }
}

// Debug logging
export let DEBUG = false;

export function setDebug(value: boolean) {
  DEBUG = value;
}

export function dp(...args: any[]) {
  if (DEBUG) {
    console.log('[DEBUG]', ...args);
  }
}