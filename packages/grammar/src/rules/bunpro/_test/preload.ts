/**
 * Preload script for bun test - starts GinzaClient once before all tests.
 * This runs in the same process as all test files, so globalThis is actually shared.
 */
import { join } from 'node:path';
import { GinzaClient } from '../../../ginza/client.js';
import { findPackageRoot } from '../../../paths.js';

declare global {
  var __sharedGinzaClient: GinzaClient | undefined;
  var __sharedGinzaClientReady: Promise<void> | undefined;
}

const pkgRoot = findPackageRoot(import.meta.url);
const client = new GinzaClient({
  python: 'python3',
  cacheDir: join(pkgRoot, '.ginza-cache'),
});
globalThis.__sharedGinzaClient = client;
globalThis.__sharedGinzaClientReady = client.start();

