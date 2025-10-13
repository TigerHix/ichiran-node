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

// INSTRUMENTATION: Track query count and log queries
let queryCount = 0;
let queryLog: Array<{query: string; params: any[]; stack?: string; timestamp: number}> = [];
let logQueries = false;
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

function createSqlConnection(spec: ConnectionSpec): postgres.Sql {
  const rawConnection = postgres({
    host: spec.host,
    port: spec.port ?? 5432,
    database: spec.database,
    user: spec.user,
    password: spec.password,
    ssl: spec.ssl ? 'require' : false,
    // Automatically convert snake_case ↔ camelCase for column names only
    transform: postgres.camel,
    // Enable prepared statements for better performance (matches Lisp make-dao behavior)
    prepare: true,
    // Auto-close idle connections to prevent hanging (especially for scripts)
    idle_timeout: 1, // Close connections idle for 1+ seconds
    max_lifetime: 60 * 5 // Max connection lifetime: 5 minutes
  });

  return new Proxy(rawConnection, {
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

export function setConnection(spec: ConnectionSpec) {
  const existing = connection;

  if (existing) {
    connection = null;
    existing.end().catch((error) => {
      console.warn('Failed to close existing Postgres connection cleanly:', error);
    });
  }

  connection = createSqlConnection(spec);
}

/**
 * Safety check: Prevents accidental operations on production database.
 * Throws an error if the database name is 'jmdict' (production).
 * Only allows operations on 'jmdict_test' or other non-production databases.
 */
export function validateDatabaseSafety(operation: string = 'operation') {
  const spec = getConnectionFromEnv();
  if (!spec) {
    throw new Error('No database connection configured. Set ICHIRAN_DB_URL environment variable.');
  }

  const dbName = spec.database.toLowerCase();

  // Block operations on production database
  if (dbName === 'jmdict') {
    throw new Error(
      `SAFETY CHECK FAILED: Cannot perform ${operation} on production database 'jmdict'.\n` +
      `Use 'jmdict_test' or another test database instead.\n` +
      `Current ICHIRAN_DB_URL: ${process.env.ICHIRAN_DB_URL}`
    );
  }

  console.log(`✓ Database safety check passed: ${dbName}`);
}

export function getConnection(): postgres.Sql {
  if (!connection) {
    const spec = getConnectionFromEnv();
    if (!spec) {
      throw new Error('No database connection configured. Set ICHIRAN_DB_URL environment variable.');
    }
    connection = createSqlConnection(spec);
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