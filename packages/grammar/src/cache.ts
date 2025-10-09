import { LRUCache } from 'lru-cache';
import { compilePattern } from './compiler.js';
import type { GrammarDefinition } from './types.js';
import type { Matcher } from './compiler.js';

let compiledGrammarCache: LRUCache<string, Matcher> = new LRUCache({ max: 500 });
let compiledCacheMisses = 0;
let compiledCacheHits = 0;

/**
 * Clear the compiled grammar cache. Useful for tests or when grammar definitions change at runtime.
 */
export function clearCompiledGrammarCache(): void {
  compiledGrammarCache.clear();
  compiledCacheHits = 0;
  compiledCacheMisses = 0;
}

/**
 * Set the capacity of the compiled grammar cache.
 */
export function setCompiledGrammarCacheCapacity(capacity: number): void {
  if (Number.isFinite(capacity) && capacity > 0) {
    compiledGrammarCache = new LRUCache({ max: Math.floor(capacity) });
  }
}

/**
 * Get cache statistics for profiling.
 */
export function getCacheStats(): { hits: number; misses: number } {
  return { hits: compiledCacheHits, misses: compiledCacheMisses };
}

/**
 * Reset cache statistics (e.g., at the start of a compile run).
 */
export function resetCacheStats(): void {
  compiledCacheHits = 0;
  compiledCacheMisses = 0;
}

/**
 * Get or compile a matcher for a grammar definition, with caching.
 * Cache key is based on grammar id and pattern JSON to detect changes.
 */
export function getCompiledMatcher(def: GrammarDefinition): Matcher {
  const patternKey = JSON.stringify(def.pattern);
  const cacheKey = `${def.id}|${patternKey}`;
  
  let matcher = compiledGrammarCache.get(cacheKey);
  if (!matcher) {
    matcher = compilePattern(def.pattern);
    compiledGrammarCache.set(cacheKey, matcher);
    compiledCacheMisses++;
  } else {
    compiledCacheHits++;
  }
  
  return matcher;
}

