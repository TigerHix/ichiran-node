export type DetailPropertyTag =
  | 'dial'
  | 'field'
  | 'misc'
  | 'pos'
  | 's_inf'
  | 'stagk'
  | 'stagr';

export interface DetailGloss {
  readonly ord: number;
  readonly text: string;
}

export interface DetailProperty {
  readonly tag: DetailPropertyTag;
  readonly ord: number;
  readonly text: string;
}

export interface DetailSense {
  readonly ord: number;
  readonly glosses: readonly DetailGloss[];
  readonly properties: readonly DetailProperty[];
}

export interface DetailForm {
  readonly route: 'kanji' | 'kana';
  readonly text: string;
  readonly ord: number;
  readonly common: number | null;
  readonly commonTags: string;
  readonly conjugatable: boolean;
  readonly nokanji: boolean;
  readonly best: string | null;
}

export interface DetailEntry {
  readonly seq: number;
  readonly forms: readonly DetailForm[];
  readonly senses: readonly DetailSense[];
}

export interface DetailRandomAccessSource {
  readonly byteLength: number;
  read(offset: number, byteLength: number): Promise<Uint8Array>;
  dispose?(): void;
}

export type DetailStoreErrorCode =
  | 'invalid-header'
  | 'unsupported-version'
  | 'corrupt-index'
  | 'corrupt-block'
  | 'out-of-range';

export class DetailStoreError extends Error {
  readonly code: DetailStoreErrorCode;

  constructor(code: DetailStoreErrorCode, message: string) {
    super(message);
    this.name = 'DetailStoreError';
    this.code = code;
  }
}
