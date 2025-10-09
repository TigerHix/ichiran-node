// Shared test setup utilities
import { beforeAll } from 'bun:test';
import { setConnection, initializeIchiran, initSuffixes, type ConnectionSpec } from '@ichiran/core';
import './load-env.js'; // Load .env file

// Helper to parse connection from env (for tests)
function getConnectionFromEnv(): ConnectionSpec | null {
  const dbUrl = process.env.ICHIRAN_TEST_DB_URL || process.env.ICHIRAN_DB_URL;
  if (!dbUrl) return null;

  try {
    const normalized = dbUrl.replace(/^postgresql:\/\//, 'postgres://');
    const url = new URL(normalized);
    const database = decodeURIComponent(url.pathname.replace(/^\//, ''));
    if (!database) throw new Error('Database name missing');

    const hostParam = url.searchParams.get('host');
    let host = url.hostname;
    if (!host && hostParam) host = decodeURIComponent(hostParam);
    if (!host) host = 'localhost';

    const portParam = url.port || url.searchParams.get('port') || undefined;
    const user = url.username ? decodeURIComponent(url.username) : '';
    const password = url.password ? decodeURIComponent(url.password) : '';

    const spec: ConnectionSpec = { user, password, host, database };
    if (portParam) {
      const parsedPort = Number(portParam);
      if (!Number.isFinite(parsedPort)) throw new Error(`Invalid port: ${portParam}`);
      spec.port = parsedPort;
    }

    const sslParam = url.searchParams.get('ssl');
    const sslMode = url.searchParams.get('sslmode');
    if (sslParam) {
      const normalizedSsl = sslParam.toLowerCase();
      if (['true', '1', 'require'].includes(normalizedSsl)) spec.ssl = true;
      else if (['false', '0', 'disable'].includes(normalizedSsl)) spec.ssl = false;
    } else if (sslMode) {
      const normalizedSslmode = sslMode.toLowerCase();
      if (['require', 'verify-ca', 'verify-full'].includes(normalizedSslmode)) spec.ssl = true;
      else if (normalizedSslmode === 'disable') spec.ssl = false;
    }
    return spec;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`Invalid database URL (${dbUrl}): ${message}`);
  }
}

let setupComplete = false;

// Initialize database connection and pre-warm all caches before running tests
export function setupTests() {
  beforeAll(async () => {
    if (setupComplete) {
      return; // Already set up
    }

    const connSpec = getConnectionFromEnv();
    if (!connSpec) {
      throw new Error('Database connection not configured. Set ICHIRAN_DB_URL environment variable.');
    }
    setConnection(connSpec);

    console.log('Database connection initialized');

    // Initialize Ichiran (registers suffix definitions)
    initializeIchiran();

    console.log('Pre-warming suffix cache...');

    const start = performance.now();
    await initSuffixes({ blocking: true });
    const elapsed = performance.now() - start;

    console.log(`Suffix cache initialized in ${elapsed.toFixed(0)}ms`);
    setupComplete = true;
  });

  // Note: We don't close the connection in afterAll because:
  // 1. Each test file registers its own afterAll hook
  // 2. The first file to complete would close the shared connection
  // 3. Subsequent test files would fail with CONNECTION_ENDED
  // The connection will be cleaned up when the process exits
}

// Helper to extract just the text from segmented results (matching assert-segment)
export function extractTexts(wordInfos: any[]): (string | symbol)[] {
  return wordInfos.map(wi => wi.type === 'gap' ? ':gap' : wi.text);
}