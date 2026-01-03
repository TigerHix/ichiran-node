/**
 * Shared GrammarEngine singleton for tests.
 * Uses globalThis to persist across module boundaries in bun test.
 */
import { afterAll, beforeAll } from 'bun:test';
import { GrammarEngine } from '../../../program.js';
import type { Ruleset } from '../../../ruleset.js';

declare global {
  var __bunproTestEngine: GrammarEngine | undefined;
  var __bunproTestEnginePromise: Promise<GrammarEngine> | undefined;
  var __bunproTestEngineRefCount: number;
}

globalThis.__bunproTestEngineRefCount ??= 0;

/**
 * Get the shared GrammarEngine instance.
 * Lazily creates one engine for all bunpro tests.
 */
export async function getSharedEngine(rulesets: Ruleset[]): Promise<GrammarEngine> {
  if (!globalThis.__bunproTestEnginePromise) {
    globalThis.__bunproTestEnginePromise = GrammarEngine.create(rulesets, {
      ginza: { python: 'python3' },
    });
    globalThis.__bunproTestEngine = await globalThis.__bunproTestEnginePromise;
  }
  return globalThis.__bunproTestEnginePromise;
}

/**
 * Setup hook for test files. Call in describe() block.
 * Returns a getter for the engine (available after beforeAll runs).
 */
export function useSharedEngine(rulesets: Ruleset[]): { get: () => GrammarEngine } {
  let engine: GrammarEngine;

  beforeAll(async () => {
    engine = await getSharedEngine(rulesets);
    globalThis.__bunproTestEngineRefCount++;
  });

  afterAll(async () => {
    globalThis.__bunproTestEngineRefCount--;
    // Close engine when last test file finishes
    if (globalThis.__bunproTestEngineRefCount === 0 && globalThis.__bunproTestEngine) {
      await globalThis.__bunproTestEngine.close();
      globalThis.__bunproTestEngine = undefined;
      globalThis.__bunproTestEnginePromise = undefined;
    }
  });

  return {
    get: () => engine,
  };
}

