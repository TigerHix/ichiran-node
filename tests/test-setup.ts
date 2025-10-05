// Shared test setup utilities
import { beforeAll } from 'bun:test';
import { getConnectionFromEnv, setConnection } from '../src/conn.js';
import { initSuffixes } from '../src/grammar/suffixCache.js';
import { initializeIchiran } from '../src/init.js';
import './load-env.js'; // Load .env file

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