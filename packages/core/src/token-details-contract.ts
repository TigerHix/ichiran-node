/** Canonical, context-aware presentation for one selected analysis token. */
export interface TokenDetails {
  readonly text: string;
  readonly reading: string;
  readonly meanings: readonly TokenMeaning[];
  readonly components: readonly TokenDetails[];
  readonly conjugations: readonly TokenConjugation[];
  /** Ranked candidates other than the selected primary candidate. */
  readonly alternatives: readonly TokenDetails[];
  readonly suffix: string | null;
  readonly counter: TokenCounter | null;
  readonly entity: boolean;
}

export interface TokenMeaning {
  readonly gloss: string;
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
