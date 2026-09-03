export type PortableAnalysisRoute = 'kana' | 'kanji';

export interface PortableAnalysisRoot {
  readonly seq: number;
  readonly form: string;
  readonly reading: string;
}

export interface PortableAnalysisInflection {
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
  readonly ordinal: number;
}

export interface PortableAnalysisComponent {
  readonly text: string;
  readonly trueText: string | null;
  readonly route: PortableAnalysisRoute;
  readonly reading: string;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly primary: boolean;
}

export interface PortableAnalysisAlternative {
  /** Request-local identity only; never persist or compare across calls, packs, or runtimes. */
  readonly candidateId: number;
  readonly text: string;
  readonly trueText: string | null;
  readonly route: PortableAnalysisRoute;
  readonly reading: string;
  readonly romanized: string;
  readonly pos: readonly string[];
  readonly score: number;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly components: readonly PortableAnalysisComponent[];
  readonly counter: readonly [string, boolean] | null;
}

export interface PortableAnalysisToken {
  /** Request-local identity only; `null` for gaps and unstable across calls, packs, or runtimes. */
  readonly candidateId: number | null;
  readonly start: number;
  readonly end: number;
  readonly text: string;
  readonly trueText: string | null;
  readonly route: PortableAnalysisRoute | 'gap';
  readonly reading: string;
  readonly romanized: string;
  readonly pos: readonly string[];
  readonly score: number;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly components: readonly PortableAnalysisComponent[];
  readonly alternatives: readonly PortableAnalysisAlternative[];
  readonly skipped: number;
  readonly entity: boolean;
  readonly counter: readonly [string, boolean] | null;
}

export interface PortableAnalysisPath {
  readonly score: number;
  readonly tokens: readonly PortableAnalysisToken[];
}

export type PortableAnalysisChunk =
  | {
      readonly type: 'misc';
      readonly start: number;
      readonly end: number;
      readonly text: string;
    }
  | {
      readonly type: 'word';
      readonly start: number;
      readonly end: number;
      readonly text: string;
      /** Independently scored paths for this basicSplit word segment. */
      readonly paths: readonly PortableAnalysisPath[];
    };

export interface PortableAnalysisResult {
  readonly input: string;
  readonly normalized: string;
  readonly computeMs: number;
  /** Exact basicSplit ownership used by legacy romanize*. */
  readonly chunks: readonly PortableAnalysisChunk[];
  /** Deterministic top-N Cartesian merge of independent word-chunk paths. */
  readonly paths: readonly PortableAnalysisPath[];
}
