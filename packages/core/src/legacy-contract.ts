export interface PortableLegacySenseJson {
  readonly pos: string;
  readonly gloss: string;
  readonly field?: string;
  readonly info?: string;
}

export interface PortableLegacyConjugationJson {
  readonly prop: readonly {
    readonly pos: string;
    readonly type: string;
    readonly fml?: true;
    readonly neg?: true;
  }[];
  readonly reading?: string;
  readonly gloss?: readonly PortableLegacySenseJson[];
  readonly readok?: boolean;
  readonly via?: readonly PortableLegacyConjugationJson[];
  readonly [PORTABLE_LEGACY_INFO]?: PortableLegacyConjugationInfoFacts;
}

export interface PortableLegacyGlossJson {
  readonly reading?: string;
  readonly text?: string;
  readonly kana?: string | readonly string[];
  readonly score?: number;
  readonly compound?: readonly string[];
  readonly components?: readonly PortableLegacyGlossJson[];
  readonly counter?: { readonly value: string; readonly ordinal: true | readonly [] };
  readonly seq?: number | readonly number[];
  readonly gloss?: readonly PortableLegacySenseJson[];
  readonly suffix?: string;
  readonly conj?: readonly PortableLegacyConjugationJson[];
  readonly alternative?: readonly PortableLegacyGlossJson[];
  readonly [PORTABLE_LEGACY_INFO]?: PortableLegacyWordInfoFacts;
}

export interface PortableLegacyWordInfoFacts {
  readonly definitionSeq: number | null;
  readonly conjugationSelection: 'default' | 'explicit' | 'root';
  readonly inflected: boolean;
}

export interface PortableLegacyConjugationInfoFacts {
  readonly flags: readonly {
    readonly negative: boolean | null;
    readonly formal: boolean | null;
  }[];
  readonly shortGloss?: string;
}

/** In-memory compatibility metadata; symbol keys do not alter detailed JSON. */
export const PORTABLE_LEGACY_INFO: unique symbol = Symbol('ichiran.legacy-info');

export type PortableLegacyTransformedToken = readonly [
  romanized: string,
  word: PortableLegacyGlossJson,
  property: unknown
];
export type PortableLegacyTransformedPath = readonly [
  words: readonly PortableLegacyTransformedToken[],
  score: number
];
export type PortableLegacyTransformedResult = readonly (
  | string
  | readonly PortableLegacyTransformedPath[]
)[];
