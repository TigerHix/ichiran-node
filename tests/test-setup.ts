// Shared test setup utilities
import { beforeAll, afterAll } from 'bun:test';
import { getConnectionFromEnv, setConnection, getConnection } from '../src/conn.js';
import { initSuffixes } from '../src/dict-grammar.js';
import './load-env.js'; // Load .env file

// Initialize database connection and pre-warm all caches before running tests
export function setupTests() {
  beforeAll(async () => {
    const connSpec = getConnectionFromEnv();
    if (!connSpec) {
      throw new Error('Database connection not configured. Set ICHIRAN_DB_URL environment variable.');
    }
    setConnection(connSpec);

    console.log('Database connection initialized');
    console.log('Pre-warming suffix cache...');

    const start = performance.now();
    await initSuffixes({ blocking: true });
    const elapsed = performance.now() - start;

    console.log(`Suffix cache initialized in ${elapsed.toFixed(0)}ms`);
  });

  // Close database connection after all tests complete
  afterAll(async () => {
    const conn = getConnection();
    await conn.end();
  });
}

// Helper to extract just the text from segmented results (matching assert-segment)
export function extractTexts(wordInfos: any[]): (string | symbol)[] {
  return wordInfos.map(wi => wi.type === 'gap' ? ':gap' : wi.text);
}