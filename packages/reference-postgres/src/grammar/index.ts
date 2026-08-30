// Grammar module barrel export
// Re-exports public API from grammar submodules

// Types
export type {
  Synergy,
  SuffixValue,
  SuffixCacheEntry,
  ParsedSuffix,
  SuffixFunction,
  ScoreValue,
  SynergyFunction,
  PenaltyFunction,
  SegfilterFunction
} from './types.js';

// Suffix Cache
export {
  SUFFIX_DESCRIPTION,
  initSuffixes,
  getSuffixDescription,
  getSuffixCache,
  getSuffixClass
} from './suffixCache.js';

// Suffix Helpers (moved to dict/ to resolve circular dependencies)
export {
  getKanaForms,
  getKanaForm,
  findWordSeq,
  findWordConjOf,
  findWordWithSuffix,
  findWordWithConjProp,
  findWordWithConjType,
  findWordWithPos,
  pairWordsByConj,
  orAsHiragana
} from '../dict/suffixHelpers.js';

// Suffix Matcher
export {
  getSuffixMap,
  getSuffixes,
  findWordSuffix
} from './suffixMatcher.js';

// Suffix definitions are loaded as side-effect (moved to dict/)
import '../dict/suffixDefinitions.js';

// Synergies
export { getSynergies } from './synergies.js';

// Penalties
export { getPenalties } from './penalties.js';

// Segfilters
export { applySegfilters } from './segfilters.js';
