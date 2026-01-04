/**
 * Shared GrammarEngine singleton for tests.
 * Uses the GinzaClient started in preload.ts.
 */
import { beforeAll } from 'bun:test';
import type { GinzaClient } from '../../../ginza/client.js';
import { GrammarEngine } from '../../../program.js';
import type { Ruleset } from '../../../ruleset.js';

declare global {
  var __sharedGinzaClient: GinzaClient | undefined;
  var __sharedGinzaClientReady: Promise<void> | undefined;
  var __bunproTestEngineRefCount: number;
}

globalThis.__bunproTestEngineRefCount ??= 0;

/**
 * Get or create a GrammarEngine using the shared GinzaClient from preload.
 */
export async function getSharedEngine(rulesets: Ruleset[]): Promise<GrammarEngine> {
  // Wait for the shared client started in preload.ts
  if (globalThis.__sharedGinzaClientReady) {
    await globalThis.__sharedGinzaClientReady;
  }
  const client = globalThis.__sharedGinzaClient;
  if (!client) {
    throw new Error('Shared GinzaClient not found. Make sure preload.ts is configured in bunfig.toml');
  }
  return GrammarEngine.create(rulesets, { client });
}

/**
 * Setup hook for test files. Call in describe() block.
 * Returns a getter for the engine (available after beforeAll runs).
 */
export function useSharedEngine(rulesets: Ruleset[]): { get: () => GrammarEngine } {
  let engine: GrammarEngine;

  beforeAll(async () => {
    engine = await getSharedEngine(rulesets);
  });

  // Don't close - the GinzaClient is shared and will be cleaned up when process exits

  return {
    get: () => engine,
  };
}

