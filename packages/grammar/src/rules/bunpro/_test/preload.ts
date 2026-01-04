/**
 * Preload script for bun test - starts GinzaClient once before all tests.
 * This runs in the same process as all test files, so globalThis is actually shared.
 */
import { GinzaClient } from '../../../ginza/client.js';

declare global {
  var __sharedGinzaClient: GinzaClient | undefined;
  var __sharedGinzaClientReady: Promise<void> | undefined;
}

const client = new GinzaClient({ python: 'python3' });
globalThis.__sharedGinzaClient = client;
globalThis.__sharedGinzaClientReady = client.start();

