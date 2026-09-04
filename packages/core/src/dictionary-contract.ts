export type DictionaryPropertyTag =
  | 'dial'
  | 'field'
  | 'misc'
  | 'pos'
  | 's_inf'
  | 'stagk'
  | 'stagr';

export type LexiconPropertyTag = Exclude<DictionaryPropertyTag, 's_inf'>;

export interface DictionaryGloss {
  readonly ord: number;
  readonly text: string;
}

export interface DictionaryProperty {
  readonly tag: DictionaryPropertyTag;
  readonly ord: number;
  readonly text: string;
}

export interface DictionarySense {
  readonly ord: number;
  readonly glosses: readonly DictionaryGloss[];
  readonly properties: readonly DictionaryProperty[];
}

export interface DictionaryForm {
  readonly route: 'kanji' | 'kana';
  readonly text: string;
  readonly ord: number;
  readonly common: number | null;
  readonly commonTags: string;
  readonly conjugatable: boolean;
  readonly nokanji: boolean;
  readonly best: string | null;
}

export interface DictionaryEntry {
  readonly seq: number;
  readonly forms: readonly DictionaryForm[];
  readonly senses: readonly DictionarySense[];
}

export interface LexiconSense {
  readonly ord: number;
  readonly properties: readonly (DictionaryProperty & { readonly tag: LexiconPropertyTag })[];
}

export interface LexiconEntry {
  readonly seq: number;
  readonly forms: readonly DictionaryForm[];
  readonly senses: readonly LexiconSense[];
}

export interface LocaleGlossGroup {
  /** Empty means an unaligned entry-wide gloss group. */
  readonly targets: readonly number[];
  readonly glosses: readonly DictionaryGloss[];
  readonly info: readonly DictionaryGloss[];
}

export interface LocaleGlossEntry {
  readonly seq: number;
  readonly groups: readonly LocaleGlossGroup[];
}

export interface DictionaryRandomAccessSource {
  readonly byteLength: number;
  read(offset: number, byteLength: number): Promise<Uint8Array>;
  dispose?(): void;
}

export type DictionaryStoreErrorCode =
  | 'invalid-header'
  | 'unsupported-version'
  | 'corrupt-index'
  | 'corrupt-block'
  | 'out-of-range';

export class DictionaryStoreError extends Error {
  readonly code: DictionaryStoreErrorCode;

  constructor(code: DictionaryStoreErrorCode, message: string) {
    super(message);
    this.name = 'DictionaryStoreError';
    this.code = code;
  }
}
