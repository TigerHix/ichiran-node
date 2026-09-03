/** Production analyzer facade. Compiler and qualification internals use subpaths. */
export * from './runtime.js';
export {
  AnalyzerInputError,
  MAX_ANALYZER_ENTITIES,
  MAX_ANALYZER_ENTITY_ABS_BOOST,
  MAX_ANALYZER_LIMIT,
  MAX_ANALYZER_TEXT_LENGTH,
  MAX_ANALYZER_WORD_LENGTH,
  validateAnalyzerEntities,
  validateAnalyzerLimit,
  validatePortableAnalyzeRequest,
  type PortableAnalyzeOptions,
  type ValidatedPortableAnalyzeOptions
} from './analyzer-options.js';
export type {
  PortableAnalysisAlternative,
  PortableAnalysisChunk,
  PortableAnalysisComponent,
  PortableAnalysisInflection,
  PortableAnalysisPath,
  PortableAnalysisResult,
  PortableAnalysisRoot,
  PortableAnalysisToken
} from './analyzer-result-contract.js';
export type { AnalyzerEntityHint } from './analyzer-types.js';
export {
  DetailStoreError,
  type DetailEntry,
  type DetailRandomAccessSource,
  type DetailStoreErrorCode
} from './details-contract.js';
export * from './release-manifest.js';
export {
  PORTABLE_LEGACY_INFO,
  type PortableLegacyConjugationInfoFacts,
  type PortableLegacyConjugationJson,
  type PortableLegacyGlossJson,
  type PortableLegacySenseJson,
  type PortableLegacyTransformedPath,
  type PortableLegacyTransformedResult,
  type PortableLegacyTransformedToken,
  type PortableLegacyWordInfoFacts
} from './legacy-contract.js';
export { joinRomanizedParts, type RomanizationName } from './romanization-contract.js';
