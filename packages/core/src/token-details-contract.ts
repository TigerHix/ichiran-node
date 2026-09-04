/** Canonical, context-aware presentation for one selected analysis token. */
export interface TokenDetails {
  readonly text: string;
  readonly reading: string;
  readonly meanings: readonly TokenMeaning[];
  readonly components: readonly TokenDetails[];
  readonly conjugations: readonly TokenConjugation[];
  /** Ranked candidates other than the selected primary candidate. */
  readonly alternatives: readonly TokenDetails[];
  /** Stable semantic identifier. Presentation belongs to the UI locale catalog. */
  readonly suffixId: TokenSuffixId | null;
  readonly counter: TokenCounter | null;
  /** Analyzer classification, independent of dictionary and presentation locale. */
  readonly entityKind: TokenEntityKind | null;
}

export type TokenEntityKind = 'proper-noun';

export type TokenSuffixId =
  | 'chau'
  | 'ha'
  | 'tai'
  | 'iru'
  | 'oru'
  | 'aru'
  | 'kuru'
  | 'oku'
  | 'kureru'
  | 'morau'
  | 'itadaku'
  | 'iku'
  | 'suru'
  | 'itasu'
  | 'sareru'
  | 'saseru'
  | 'rou'
  | 'ii'
  | 'mo'
  | 'sugiru'
  | 'nikui'
  | 'gatai'
  | 'sa'
  | 'tsutsu'
  | 'tsutsuaru'
  | 'uru'
  | 'sou'
  | 'nai'
  | 'ra'
  | 'kudasai'
  | 'yagaru'
  | 'naru'
  | 'desu'
  | 'desho'
  | 'tosuru'
  | 'garu'
  | 'me'
  | 'gai'
  | 'tasou'
  | 'polite-prefix'
  | 'particle-ni'
  | 'particle-ka'
  | 'particle-e'
  | 'particle-o'
  | 'particle-no'
  | 'particle-to'
  | 'particle-kara';

export interface TokenMeaning {
  readonly gloss: string;
  /** Unique POS codes in stable canonical order; source order has no semantic meaning. */
  readonly pos: readonly string[];
  readonly fields: readonly string[];
  readonly info: string | null;
}

export interface TokenDetailForm {
  readonly text: string;
  readonly reading: string;
}

export interface TokenConjugation {
  readonly root: TokenDetailForm | null;
  readonly properties: readonly TokenConjugationProperty[];
  readonly meanings: readonly TokenMeaning[];
  readonly via: readonly TokenConjugation[];
}

export interface TokenConjugationProperty {
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean;
  readonly formal: boolean;
}

export interface TokenCounter {
  readonly value: string;
  readonly ordinal: boolean;
}
