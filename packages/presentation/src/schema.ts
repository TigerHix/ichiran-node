import type { TokenSuffixId } from '@ichiran/core';

export const PRESENTATION_LOCALES = ['en', 'zh-Hans'] as const;
export type PresentationLocale = typeof PRESENTATION_LOCALES[number];

export const UI_MESSAGE_IDS = [
  'home', 'settings', 'analyzerData', 'interfaceLanguage', 'onThisDevice', 'removeData',
  'licenses', 'installTitle', 'installIntro', 'preparing', 'staleData', 'incompleteData',
  'insufficientStorage', 'clearFailed', 'downloadFailed', 'downloading', 'checkingFiles',
  'savingDevice', 'openingAnalyzer', 'reinstall', 'retry', 'install', 'download', 'stored', 'byteProgress',
  'privacy', 'examples', 'trySentence', 'reading', 'word', 'detailsFailed',
  'romanizationFailed', 'replaceFailed', 'japaneseText', 'composerIntro', 'clearJapanese',
  'analyzing', 'analyze', 'analysisFailed', 'tryAgain', 'analysisResult', 'analysis',
  'definitions', 'english', 'simplifiedChinese', 'romanize', 'hideRomaji', 'otherParses',
  'analyzeHint', 'enterHint', 'wordDetails', 'close', 'copyFailed', 'runsOnDevice',
  'selectWord', 'selectedText', 'copied', 'copy', 'copySelected', 'closeDetails',
  'loadingDetails', 'negative', 'formal', 'counter', 'ordinalCounter', 'properNoun',
  'structure', 'conjugations', 'alternatives', 'unknownConjugation', 'confirmRemove',
  'unsupportedTitle', 'unsupportedIntro', 'corruptData', 'reloadStatus'
] as const;
export type UiMessageId = typeof UI_MESSAGE_IDS[number];

export const SAMPLE_IDS = [
  'everyday', 'inflection', 'counters', 'numbers', 'ambiguous',
  'mixedScripts', 'punctuation', 'colloquial', 'compound', 'kana'
] as const;
export type SampleId = typeof SAMPLE_IDS[number];

export type UiCatalog = Readonly<Record<UiMessageId, string>>;
export type SampleCatalog = Readonly<Record<SampleId, string>>;
export type StringCatalog = Readonly<Record<string, string>>;
export type SuffixCatalog = Readonly<Record<TokenSuffixId, string>>;

export interface PresentationCatalog {
  readonly ui: UiCatalog;
  readonly samples: SampleCatalog;
  readonly pos: StringCatalog;
  readonly fields: StringCatalog;
  readonly conjugations: Readonly<Record<number, string>>;
  readonly suffixes: SuffixCatalog;
}

export const CATALOG_CONTEXT = {
  ui: 'Browser and native application chrome. Keep labels concise and action-oriented.',
  samples: 'Short labels for Japanese analyzer example sentences.',
  pos: 'Learner-facing Japanese part-of-speech terminology. Preserve Japanese class names where useful.',
  fields: 'Compact subject-domain labels attached to dictionary senses.',
  conjugations: 'Learner-facing names for Japanese inflection forms.',
  suffixes: 'Explanations for analyzer-recognized Japanese suffix constructions.'
} as const;
