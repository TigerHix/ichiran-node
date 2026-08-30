// Word lookup context using AsyncLocalStorage
// Provides findWordFull to suffix code without creating circular dependencies

import { AsyncLocalStorage } from 'async_hooks';

type WordLookupFn = (word: string, options?: any) => Promise<any[]>;

const lookupContext = new AsyncLocalStorage<WordLookupFn>();

/**
 * Get current word lookup function from context.
 * Throws if no context is set.
 */
export function getWordLookup(): WordLookupFn {
  const fn = lookupContext.getStore();
  if (!fn) {
    throw new Error('Word lookup context not set - missing withWordLookup wrapper');
  }
  return fn;
}

/**
 * Run function within a word lookup context.
 *
 * @param lookupFn - The word lookup function to make available
 * @param fn - Async function to run within context
 */
export function withWordLookup<T>(
  lookupFn: WordLookupFn,
  fn: () => Promise<T>
): Promise<T> {
  return lookupContext.run(lookupFn, fn);
}
