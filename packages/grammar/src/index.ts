// @ichiran/grammar - Grammar runtime, predicates, and definitions

// Re-export from grammarMatcher
export * from './grammarMatcher/types.js';
export * from './grammarMatcher/catalog.js';
export * from './grammarMatcher/runtime.js';

// Main exports for external use
export type { CompiledGrammar } from './grammarMatcher/runtime.js';
export { matchSentence, analyzeSentence } from './grammarMatcher/runtime.js';
export { grammarCatalog } from './grammarMatcher/catalog.js';

