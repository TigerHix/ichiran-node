/** Browser-analyzer score and path data. These are runtime facts, not DB rows. */

export type AnalyzerWordRoute = 'kanji' | 'kana';

export interface AnalyzerConjugationProperty {
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
}

export interface AnalyzerConjugation {
  /** Generated entry sequence. Kept because legacy scoring distinguishes it from `from`. */
  readonly seq: number;
  /** Root entry sequence. */
  readonly from: number;
  /** Intermediate generated sequence for a secondary conjugation. */
  readonly via: number | null;
  readonly property: AnalyzerConjugationProperty;
}

/** Aggregate facts for the exact sequence set selected by legacy scoring. */
export interface AnalyzerSequenceFacts {
  /** True only when the selected, non-empty sequence set is entirely archived. */
  readonly allArchived: boolean;
  /** At least one selected sequence has an `uk` sense property. */
  readonly preferKana: boolean;
  /** At least one selected `uk` property belongs to a sense with ordinal zero. */
  readonly preferKanaOnOrdinalZero: boolean;
}

export interface AnalyzerEntryScoreFacts {
  readonly root: boolean;
  readonly nKanji: number;
  readonly primaryNokanji: boolean;
}

/**
 * A score modifier flattened from the two modifier kinds used by suffixes.
 *
 * Numeric modifiers contribute `propertyScore * multiplier * extraMorae`.
 * Suffix score functions are constants in the current analyzer and contribute
 * `constant` once, irrespective of the added length.
 */
export interface AnalyzerScoreModifier {
  readonly multiplier: number;
  readonly constant: number;
}

export interface AnalyzerAdditiveSplit {
  readonly kind: 'add';
  readonly score: number;
}

export interface AnalyzerProportionalSplit {
  readonly kind: 'proportional';
  readonly score: number;
}

export interface AnalyzerPartsSplit {
  readonly kind: 'parts';
  readonly score: number;
  readonly parts: readonly AnalyzerScoreCandidate[];
  /**
   * Exact rematerialization of the last part after legacy overflow truncation.
   * Omit when lookup facts are unchanged by the mechanical text truncation.
   */
  readonly truncatedLast?: AnalyzerScoreCandidate;
}

export type AnalyzerScoreSplit =
  | AnalyzerAdditiveSplit
  | AnalyzerProportionalSplit
  | AnalyzerPartsSplit;

export interface AnalyzerSuruBreakFacts {
  /** Exact suffix spelling returned by the suffix matcher. */
  readonly suffixText: string;
  /** Already-materialized candidate stored with that suffix match. */
  readonly candidate: AnalyzerScoreCandidate;
}

/** A simple/proxy/counter word after all lookup-only facts have been resolved. */
export interface AnalyzerWordScoreFacts {
  readonly kind: 'word' | 'counter';
  /** Text used by scoring (`getText()` for counters). */
  readonly text: string;
  /** Source text used for katakana detection (proxy words keep their source). */
  readonly trueText: string;
  /** Whether truncating a split part also truncates `trueText`. False for proxies. */
  readonly trueTextFollowsText: boolean;
  readonly route: AnalyzerWordRoute;
  readonly seq: number | null;
  readonly ord: number;
  readonly common: number | null;
  readonly nokanji: boolean;
  readonly entry: AnalyzerEntryScoreFacts | null;
  /** True for an explicit conjugation selector other than `:root`. */
  readonly conjugationOnly: boolean;
  readonly conjugations: readonly AnalyzerConjugation[];
  /** Non-archived POS union over `[seq, ...from]`; counters ignore this field. */
  readonly positions: readonly string[];
  /** Aggregate for `[seq]`, selected only for an unextended root/counter. */
  readonly self: AnalyzerSequenceFacts;
  /** Aggregate for `[seq, ...from]`. */
  readonly lineage: AnalyzerSequenceFacts;
  /** Best source common rank selected by legacy original-text inheritance. */
  readonly inheritedCommon: number | null;
  /** Minimum source ordinal selected by legacy original-text inheritance. */
  readonly inheritedOrd: number | null;
  readonly split: AnalyzerScoreSplit | null;
  readonly suruBreak: AnalyzerSuruBreakFacts | null;
}

/** A suffix/compound candidate whose score is based on one materialized word. */
export interface AnalyzerCompoundScoreFacts {
  readonly kind: 'compound';
  readonly text: string;
  readonly base: AnalyzerScoreCandidate;
  readonly modifier: AnalyzerScoreModifier;
  /** Legacy compound info reports conjugations from its final component. */
  readonly conjugations: readonly AnalyzerConjugation[];
  readonly suruBreak: AnalyzerSuruBreakFacts | null;
}

export type AnalyzerScoreCandidate = AnalyzerWordScoreFacts | AnalyzerCompoundScoreFacts;

export const ANALYZER_SCORE_FLAG_STRONG = 1 << 0;
export const ANALYZER_SCORE_FLAG_PRIMARY = 1 << 1;
export const ANALYZER_SCORE_FLAG_COMMON = 1 << 2;
export const ANALYZER_SCORE_FLAG_LONG = 1 << 3;

export interface AnalyzerScoreBreakdown {
  readonly propertyScore: number;
  readonly kanjiBreak: readonly number[] | null;
  readonly useLengthBonus: number;
  /** `null` for no split and proportional splits, matching the current analyzer. */
  readonly split: number | readonly [number, ...number[]] | null;
}

export interface AnalyzerScoreInfo {
  readonly positions: readonly string[];
  readonly seqSet: readonly number[];
  readonly conjugations: readonly AnalyzerConjugation[];
  readonly common: number | null;
  readonly breakdown: AnalyzerScoreBreakdown;
  /** Bitwise combination of the four `ANALYZER_SCORE_FLAG_*` constants. */
  readonly flags: number;
}

export interface AnalyzerScoreResult {
  readonly score: number;
  readonly info: AnalyzerScoreInfo;
}

export interface AnalyzerEntityHint {
  readonly start: number;
  readonly end: number;
  readonly boost?: number;
}

export type AnalyzerRuleWordKind = 'simple' | 'proxy' | 'compound' | 'counter';

/** Minimal word-shape + score facts consumed by analyzer-internal pair rules. */
export interface AnalyzerSegmentRuleFacts {
  readonly text: string;
  readonly wordKind: AnalyzerRuleWordKind;
  readonly scoreInfo: AnalyzerScoreInfo | null;
  /** Sequence of the final compound component, or null for a proxy/non-compound. */
  readonly compoundEndSeq: number | null;
  readonly compoundEndText: string | null;
}

/** One scored dictionary/suffix/counter/entity alternative in a surface span. */
export interface AnalyzerSegment {
  readonly candidateId: number;
  readonly start: number;
  readonly end: number;
  readonly score: number;
  readonly common: number | null;
  readonly entity: boolean;
  /** May be omitted when pair rules are not requested. */
  readonly rules?: AnalyzerSegmentRuleFacts;
}

/** Alternatives sharing one input span. `matches` is the pre-cull match count. */
export interface AnalyzerSegmentGroup {
  readonly groupId: number;
  readonly start: number;
  readonly end: number;
  readonly segments: readonly AnalyzerSegment[];
  readonly matches: number;
}

export interface AnalyzerPathAdjustment {
  readonly score: number;
  readonly start: number;
  readonly end: number;
  readonly description: string;
  readonly connector: string;
}

export type AnalyzerPathPart = AnalyzerSegmentGroup | AnalyzerPathAdjustment;

/** Exact shape produced by the current pairwise filter/penalty/synergy stage. */
export interface AnalyzerPathTransition {
  readonly right: AnalyzerSegmentGroup;
  readonly adjustment?: AnalyzerPathAdjustment;
  readonly left: AnalyzerSegmentGroup;
}

export interface AnalyzerPathResult {
  readonly score: number;
  /** Chronological groups/adjustments. */
  readonly parts: readonly AnalyzerPathPart[];
}

export type AnalyzerInitialResolver = (
  group: AnalyzerSegmentGroup
) => readonly AnalyzerSegmentGroup[];

export type AnalyzerTransitionResolver = (
  left: AnalyzerSegmentGroup,
  right: AnalyzerSegmentGroup
) => readonly AnalyzerPathTransition[];
