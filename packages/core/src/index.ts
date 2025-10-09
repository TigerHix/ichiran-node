// @ichiran/core - Core segmentation, dict, romanization, and connection primitives

// Connection primitives (no env parsing)
export {
  type ConnectionSpec,
  setConnection,
  getConnection,
  withDb,
  defineCache,
  initAllCaches,
  resetCache,
  resetAllCaches,
  resetQueryCount,
  getQueryCount,
  enableQueryLogging,
  disableQueryLogging,
  getQueryLog,
  getQuerySummary,
  setDebug,
  dp,
  DEBUG,
  validateDatabaseSafety
} from './conn.js';

// Romanization
export {
  romanize,
  romanizeStar,
  romanizeWordGeo,
  type RomanizeStarResult,
  type RomanizeStarResultTokenTuple,
  type RomanizeStarResultSegment
} from './romanize.js';

// Presentation transforms
export {
  transformRomanizeStarResult,
  type TransformedRomanizeStarResult,
  type TransformedRomanizeStarResultTokenTuple
} from './presentation/transformers.js';

// Dict types and functions
export type { ConjInfoJson, WordInfoGlossJson } from './dict/presentation.js';
export { WordInfo, dictSegment, simpleSegment } from './dict/presentation.js';
export { printPerfCountersAndReset } from './dict/profiling.js';
export { findWordWithPos } from './dict/suffixHelpers.js';
export { getConjDescription } from './dict/conj-description.js';

// Shared types
export type * from './types.js';

// Character utilities
export { 
  CHAR_CLASS_HASH,
  MODIFIER_CHARACTERS,
  voiceChar,
  basicSplit,
  normalize,
  simplifyNgrams,
  testWord
} from './characters.js';

// Kanji utilities
export * from './kanji.js';

// Init functions
export { initializeIchiran, isInitialized, resetInitialization } from './init.js';
export { initSuffixes } from './grammar/suffixCache.js';

