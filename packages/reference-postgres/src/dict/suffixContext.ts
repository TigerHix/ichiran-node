// Suffix lookup context using AsyncLocalStorage
// Replaces Lisp's dynamic variables (*suffix-map-temp*, *suffix-next-end*)
// with idiomatic TypeScript pattern matching substringHash in lookup.ts

import { AsyncLocalStorage } from 'async_hooks';
import type { ParsedSuffix } from '../grammar/types.js';

/**
 * Context for suffix lookup optimization.
 *
 * When set:
 * - suffixMap: Pre-computed suffix map from top-level segmentation
 * - suffixNextEnd: Position in original string for suffix lookup
 *
 * When cleared (null passed to withSuffixContext):
 * - Forces on-demand suffix computation
 * - Used by suffix handlers to prevent infinite recursion
 */
export interface SuffixLookupContext {
  suffixMap?: Map<number, ParsedSuffix[]>;
  suffixNextEnd?: number;
}

const suffixContext = new AsyncLocalStorage<SuffixLookupContext>();

/**
 * Get current suffix lookup context.
 * Returns undefined if no context is set.
 */
export function getSuffixContext(): SuffixLookupContext | undefined {
  return suffixContext.getStore();
}

/**
 * Run function within a suffix lookup context.
 *
 * @param ctx - Context to set, or null to explicitly clear context
 * @param fn - Async function to run within context
 *
 * @example
 * // Set context (like Lisp's (let ((*suffix-map-temp* map)) ...))
 * await withSuffixContext({ suffixMap, suffixNextEnd: end }, () => findWordFull(part));
 *
 * @example
 * // Clear context (like Lisp's (let ((*suffix-map-temp* nil)) ...))
 * await withSuffixContext(null, () => findWordWithConjProp(root, filter));
 */
export function withSuffixContext<T>(
  ctx: SuffixLookupContext | null,
  fn: () => Promise<T>
): Promise<T> {
  if (ctx === null) {
    // Explicitly clear context - use empty object
    // This prevents suffix handlers from using parent context
    return suffixContext.run({}, fn);
  }
  return suffixContext.run(ctx, fn);
}
