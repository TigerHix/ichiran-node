/** The complete production surface. Compiler, release, and qualification APIs use subpaths. */
export {
  ANALYZER_WASM_URL,
  Analyzer,
  AnalyzerError,
  type AnalyzeOptions,
  type AnalyzerErrorCode,
  type AnalyzerSource,
  type RandomAccessSource,
  type RomanizeOptions,
  type TokenDetailsOptions
} from './runtime.js';
export type {
  PortableAnalysisAlternative as AnalysisAlternative,
  PortableAnalysisChunk as AnalysisChunk,
  PortableAnalysisComponent as AnalysisComponent,
  PortableAnalysisInflection as AnalysisInflection,
  PortableAnalysisPath as AnalysisPath,
  PortableAnalysisResult as AnalysisResult,
  PortableAnalysisRoot as AnalysisRoot,
  PortableAnalysisRoute as AnalysisRoute,
  PortableAnalysisToken as AnalysisToken
} from './analyzer-result-contract.js';
export type { AnalyzerEntityHint as EntityHint } from './analyzer-types.js';
export type {
  DetailEntry as DictionaryEntry,
  DetailForm as DictionaryForm,
  DetailGloss as DictionaryGloss,
  DetailProperty as DictionaryProperty,
  DetailPropertyTag as DictionaryPropertyTag,
  DetailSense as DictionarySense
} from './details-contract.js';
export type { RomanizationName as RomanizationScheme } from './romanization-contract.js';
export type {
  TokenConjugation,
  TokenConjugationProperty,
  TokenCounter,
  TokenDetailForm,
  TokenDetails,
  TokenMeaning
} from './token-details-contract.js';
