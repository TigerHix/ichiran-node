import { countCharClass, moraLength } from './characters.js';
import {
  ANALYZER_SCORE_FLAG_COMMON,
  ANALYZER_SCORE_FLAG_LONG,
  ANALYZER_SCORE_FLAG_PRIMARY,
  ANALYZER_SCORE_FLAG_STRONG,
  type AnalyzerConjugation,
  type AnalyzerScoreCandidate,
  type AnalyzerScoreInfo,
  type AnalyzerScoreModifier,
  type AnalyzerScoreResult,
  type AnalyzerSegment,
  type AnalyzerSuruBreakFacts,
  type AnalyzerWordScoreFacts
} from './analyzer-types.js';

export const ANALYZER_SCORE_CUTOFF = 5;
export const ANALYZER_IDENTICAL_WORD_SCORE_CUTOFF = 1 / 2;
export const ANALYZER_SEGMENT_SCORE_CUTOFF = 2 / 3;

export const ZERO_ANALYZER_SCORE_MODIFIER: AnalyzerScoreModifier = Object.freeze({
  multiplier: 0,
  constant: 0
});

const SKIP_WORDS = new Set([
  2822120, 2013800, 2108590, 2029040, 2428180, 2654250,
  2561100, 2210270, 2210710, 2257550, 2210320, 2017560, 2394890,
  2194000, 2568000, 2537250, 2760890, 2831062, 2831063, 2029030,
  2568020, 900000, 2827357
]);

const FINAL_PARTICLES = new Set([
  2017770, 2425930, 2130430, 2029130, 2834812, 2718360, 2201380,
  2722170, 2751630
]);

const SEMI_FINAL_PARTICLES = new Set([
  ...FINAL_PARTICLES,
  2029120, 2086640, 2029110, 2029080, 2029100
]);

const NON_FINAL_PARTICLES = new Set([2139720]);
const COPULAE = new Set([2089020]);

const NO_KANJI_BREAK_PENALTY = new Set([
  1169870, 1198360, 1277450, 2028980, 1423000, 1164690, 1587040,
  2827864
]);

const LENGTH_COEFFICIENTS = {
  strong: [0, 1, 8, 24, 40, 60],
  weak: [0, 1, 4, 9, 16, 25, 36],
  tail: [0, 4, 9, 16, 24],
  ltail: [0, 4, 12, 18, 24]
} as const;

export interface AnalyzerScoreOptions {
  readonly final?: boolean;
  readonly useLength?: number;
  readonly modifier?: AnalyzerScoreModifier;
  readonly kanjiBreak?: readonly number[];
}

function lengthMultiplierCoeff(
  length: number,
  classType: 'strong' | 'weak' | 'tail' | 'ltail'
): number {
  const coefficients = LENGTH_COEFFICIENTS[classType];
  if (length > 0 && length < coefficients.length) return coefficients[length]!;
  return Math.floor(length * (coefficients[coefficients.length - 1]! / (coefficients.length - 1)));
}

function isWeakConjugation(conjugation: AnalyzerConjugation): boolean {
  const property = conjugation.property;
  return property.type === 51
    || property.type === 52
    || property.type === 53
    || property.type === 54
    || (property.type === 9 && property.negative === true);
}

function isSkippedConjugation(conjugation: AnalyzerConjugation): boolean {
  const property = conjugation.property;
  return (property.type === 10 && property.negative === true)
    || (property.type === 3 && property.negative === true && property.formal === true)
    || (property.pos === 'vs-s' && property.type === 5);
}

function selectedConjugations(
  conjugations: readonly AnalyzerConjugation[]
): { readonly secondary: boolean; readonly values: readonly AnalyzerConjugation[] } {
  const secondary = conjugations.length > 0
    && conjugations.every((conjugation) => conjugation.via !== null);
  return {
    secondary,
    values: secondary
      ? conjugations
      : conjugations.filter((conjugation) => conjugation.via === null)
  };
}

function makeEmptyScoreInfo(): AnalyzerScoreInfo {
  return {
    positions: [],
    seqSet: [],
    conjugations: [],
    common: null,
    breakdown: {
      propertyScore: 0,
      kanjiBreak: null,
      useLengthBonus: 0,
      split: null
    },
    flags: 0
  };
}

function truncateSplitPart(
  part: AnalyzerScoreCandidate,
  text: string
): AnalyzerScoreCandidate {
  if (part.kind === 'compound') return { ...part, text };
  return {
    ...part,
    text,
    trueText: part.trueTextFollowsText ? text : part.trueText
  };
}

function scoreModifier(
  modifier: AnalyzerScoreModifier,
  propertyScore: number,
  extraMorae: number
): number {
  return propertyScore * modifier.multiplier * extraMorae + modifier.constant;
}

function scoreKanjiBreak(
  candidate: AnalyzerScoreCandidate,
  breakPositions: readonly number[],
  score: number,
  info: AnalyzerScoreInfo,
  text: string,
  options: AnalyzerScoreOptions
): number {
  const end = breakPositions.length > 1
    ? 'both'
    : breakPositions[0] === 0 ? 'beg' : 'end';

  if (
    info.seqSet.some((seq) => NO_KANJI_BREAK_PENALTY.has(seq))
    || (end === 'beg' && text.startsWith('す'))
  ) {
    return score;
  }

  const hasSuruPosition = info.positions.includes('vs-s') || info.positions.includes('v5s');
  const suruBreak: AnalyzerSuruBreakFacts | null = candidate.suruBreak;
  if (hasSuruPosition && suruBreak !== null) {
    const offset = moraLength(text) - moraLength(suruBreak.suffixText);
    const suffixResult = scoreAnalyzerCandidate(suruBreak.candidate, {
      useLength: options.useLength ? options.useLength - offset : undefined,
      modifier: options.modifier
    });
    return Math.min(score, suffixResult.score + 50);
  }

  let bonus = 0;
  if (end === 'beg' && info.positions.includes('num')) bonus += 5;
  if (end === 'beg' && (info.positions.includes('suf') || info.positions.includes('n-suf'))) {
    bonus += 10;
  }
  if (end === 'end' && info.positions.includes('pref')) bonus += 12;

  return score >= ANALYZER_SCORE_CUTOFF
    ? Math.max(ANALYZER_SCORE_CUTOFF, Math.ceil(score / 2) + bonus)
    : score;
}

function scoreCompound(
  candidate: Extract<AnalyzerScoreCandidate, { readonly kind: 'compound' }>,
  options: AnalyzerScoreOptions
): AnalyzerScoreResult {
  const baseOptions: AnalyzerScoreOptions = {
    useLength: moraLength(candidate.text),
    modifier: candidate.modifier
  };
  const base = scoreAnalyzerCandidate(candidate.base, baseOptions);
  const info: AnalyzerScoreInfo = {
    ...base.info,
    conjugations: candidate.conjugations
  };
  const breakPositions = options.kanjiBreak;
  const score = breakPositions && breakPositions.length > 0
    ? scoreKanjiBreak(candidate, breakPositions, base.score, info, candidate.text, baseOptions)
    : base.score;
  return { score, info };
}

function scoreWord(
  candidate: AnalyzerWordScoreFacts,
  options: AnalyzerScoreOptions
): AnalyzerScoreResult {
  const final = options.final;
  const useLength = options.useLength;
  const modifier = options.modifier ?? ZERO_ANALYZER_SCORE_MODIFIER;
  const kanjiBreak = options.kanjiBreak;
  const counter = candidate.kind === 'counter';
  const kanji = candidate.route === 'kanji';
  const katakana = !kanji && countCharClass(candidate.trueText, 'katakana-uniq') > 0;
  const nKanji = countCharClass(candidate.text, 'kanji');
  const length = Math.max(1, moraLength(candidate.text));
  const seq = candidate.seq;
  let ord = candidate.ord;

  const root = counter || Boolean(
    candidate.entry
    && !candidate.conjugationOnly
    && candidate.entry.root
  );
  const selected = selectedConjugations(candidate.conjugations);
  const conjugations = selected.values;
  const conjugationFrom = conjugations.map((conjugation) => conjugation.from);
  const conjugationTypes = conjugations.map((conjugation) => conjugation.property.type);
  const conjugationTypesMatter = root
    || useLength !== undefined
    || !conjugations.every(isWeakConjugation);
  const seqSet = seq ? [seq, ...conjugationFrom] : [];

  const useSelfFacts = Boolean(seq && root && !useLength);
  const sequenceFacts = useSelfFacts ? candidate.self : candidate.lineage;
  const hasScoringSequence = useSelfFacts ? Boolean(seq) : seqSet.length > 0;
  const archived = hasScoringSequence && sequenceFacts.allArchived;
  const preferKana = sequenceFacts.preferKana;
  const positions = counter ? ['ctr'] : candidate.positions;

  let common = candidate.conjugationOnly ? null : candidate.common;
  let commonOf = common;
  let isCommon = common !== null;

  const particle = positions.includes('prt');
  const semiFinalParticle = seq !== null && SEMI_FINAL_PARTICLES.has(seq);
  const nonFinalParticle = seq !== null && NON_FINAL_PARTICLES.has(seq);
  const pronoun = positions.includes('pn');
  const copulaDa = seqSet.some((value) => COPULAE.has(value));

  const long = length > (
    kanji && !preferKana && (
      (root && conjugations.length === 0)
      || (Boolean(useLength) && conjugationTypes.includes(13))
    ) ? 2
      : isCommon && common !== null && common > 0 && common < 10 ? 2
        : (conjugationTypes.includes(3) || conjugationTypes.includes(9)) && !useLength ? 4
          : 3
  );
  const noCommonBonus = particle
    || !conjugationTypesMatter
    || (!long && positions.length === 1 && positions[0] === 'int');

  if (
    seqSet.some((value) => SKIP_WORDS.has(value))
    || (!final && seq !== null && FINAL_PARTICLES.has(seq))
    || (!root && conjugations.length > 0 && conjugations.every(isSkippedConjugation))
  ) {
    return { score: 0, info: makeEmptyScoreInfo() };
  }

  if (conjugations.length > 0 && !(ord === 0 && isCommon)) {
    if (!isCommon && candidate.inheritedCommon !== null) {
      common = 0;
      isCommon = true;
      commonOf = candidate.inheritedCommon;
    }
    if (candidate.inheritedOrd !== null && candidate.inheritedOrd < ord) {
      ord = candidate.inheritedOrd;
    }
  }

  let primary = false;
  if (!archived) {
    primary = candidate.entry === null
      || (preferKana && conjugationTypesMatter && !kanji
        && (!candidate.entry.primaryNokanji || candidate.nokanji))
      || ((ord === 0 || copulaDa) && (kanji || conjugationTypesMatter)
        && ((kanji && !preferKana)
          || (isCommon && pronoun)
          || candidate.entry.nKanji === 0))
      || (preferKana && kanji && ord === 0 && !sequenceFacts.preferKanaOnOrdinalZero);
  }

  let score = 1;
  if (primary) {
    score += long ? 10
      : selected.secondary && !kanji ? 2
        : isCommon && conjugationTypesMatter ? 5
          : preferKana || candidate.entry === null || candidate.entry.nKanji === 0 ? 3
            : 2;
  }

  if (particle && (final || !semiFinalParticle)) {
    score += 2;
    if (isCommon) score += 2 + length;
    if (final && !nonFinalParticle) {
      if (primary) score += 5;
      else if (semiFinalParticle) score += 2;
    }
  }

  if (isCommon && !noCommonBonus && common !== null) {
    let commonBonus = selected.secondary && !useLength
      ? kanji && primary ? 4 : 2
      : long || copulaDa || (root && (kanji || (primary && length > 2)))
        ? common === 0 ? 10 : !primary ? Math.max(15 - common, 10) : Math.max(20 - common, 10)
        : kanji ? 8
          : primary ? 4
            : length > 2 || (common > 0 && common < 10) ? 3 : 2;
    if (commonBonus >= 10 && conjugationTypes.includes(10)) commonBonus -= 4;
    score += commonBonus;
  }

  if (long) score = Math.max(length, score);
  if (kanji) {
    score = Math.max(archived ? 3 : 5, score);
    if (long && (nKanji > 1 || length > 4)) score += 2;
  }
  if (counter) score = Math.max(5, score);

  let propertyScore = score;
  score = propertyScore * (
    lengthMultiplierCoeff(length, kanji || katakana ? 'strong' : 'weak')
    + (nKanji > 1 ? (nKanji - 1) * 5 : 0)
  );

  let useLengthBonus = 0;
  if (useLength) {
    const extraMorae = useLength - length;
    useLengthBonus = propertyScore * lengthMultiplierCoeff(
      extraMorae,
      length > 3 && (kanji || katakana) ? 'ltail' : 'tail'
    );
    useLengthBonus += scoreModifier(modifier, propertyScore, extraMorae);
    score += useLengthBonus;
  }

  let splitInfo: number | readonly [number, ...number[]] | null = null;
  if (!counter && candidate.split !== null) {
    if (candidate.split.kind === 'add') {
      score += candidate.split.score;
      splitInfo = candidate.split.score;
    } else if (candidate.split.kind === 'proportional') {
      const adjustedPropertyScore = Math.max(1, propertyScore + candidate.split.score);
      score = Math.ceil((score * adjustedPropertyScore) / propertyScore);
      propertyScore = adjustedPropertyScore;
    } else {
      const partScores: number[] = [];
      let sourceLength = 0;
      let sourceMorae = 0;
      for (let index = 0; index < candidate.split.parts.length; index++) {
        const part = candidate.split.parts[index]!;
        const last = index === candidate.split.parts.length - 1;
        const partLength = part.text.length;
        sourceLength += partLength;
        const partMorae = moraLength(part.text);
        sourceMorae += partMorae;

        const scorePart = last && sourceLength > candidate.text.length
          ? candidate.split.truncatedLast ?? truncateSplitPart(
              part,
              part.text.slice(0, Math.max(1, partLength + candidate.text.length - sourceLength))
            )
          : part;
        const partResult = scoreAnalyzerCandidate(scorePart, {
          final: Boolean(final && last),
          useLength: last && useLength ? partMorae + useLength - sourceMorae : undefined,
          modifier: last ? modifier : ZERO_ANALYZER_SCORE_MODIFIER
        });
        partScores.push(partResult.score);
      }
      splitInfo = [candidate.split.score, ...partScores];
      score = candidate.split.score + partScores.reduce((sum, value) => sum + value, 0);
    }
  }

  let flags = 0;
  if (kanji || katakana) flags |= ANALYZER_SCORE_FLAG_STRONG;
  if (primary) flags |= ANALYZER_SCORE_FLAG_PRIMARY;
  if (isCommon) flags |= ANALYZER_SCORE_FLAG_COMMON;
  if (long) flags |= ANALYZER_SCORE_FLAG_LONG;

  const info: AnalyzerScoreInfo = {
    positions,
    seqSet,
    conjugations,
    common: isCommon ? commonOf : null,
    breakdown: {
      propertyScore,
      kanjiBreak: kanjiBreak ?? null,
      useLengthBonus,
      split: splitInfo
    },
    flags
  };

  if (kanjiBreak && kanjiBreak.length > 0) {
    score = scoreKanjiBreak(candidate, kanjiBreak, score, info, candidate.text, options);
  }
  return { score, info };
}

/** Score one fully materialized candidate without DB access or hidden caches. */
export function scoreAnalyzerCandidate(
  candidate: AnalyzerScoreCandidate,
  options: AnalyzerScoreOptions = {}
): AnalyzerScoreResult {
  return candidate.kind === 'compound'
    ? scoreCompound(candidate, options)
    : scoreWord(candidate, options);
}

/** Legacy common-rank ordering used as the stable secondary cull order. */
export function compareAnalyzerCommon(left: number | null, right: number | null): boolean {
  if (right === null) return left !== null;
  if (right === 0) return left !== null && left > 0;
  return left !== null && left > 0 && left < right;
}

/**
 * Stable score/common sort followed by the current 1/2-of-best cull.
 * The caller is expected to remove candidates scoring below 5 first.
 */
export function cullAnalyzerSegments(segments: readonly AnalyzerSegment[]): AnalyzerSegment[] {
  if (segments.length === 0) return [];
  const sorted = [...segments];
  sorted.sort((left, right) => {
    if (compareAnalyzerCommon(left.common, right.common)) return -1;
    if (compareAnalyzerCommon(right.common, left.common)) return 1;
    return 0;
  });
  sorted.sort((left, right) => right.score - left.score);
  const cutoff = sorted[0]!.score * ANALYZER_IDENTICAL_WORD_SCORE_CUTOFF;
  return sorted.filter((segment) => segment.score >= cutoff);
}

/** The exact lookup-stage cutoff plus cull sequence. */
export function filterAndCullAnalyzerSegments(
  segments: readonly AnalyzerSegment[]
): AnalyzerSegment[] {
  return cullAnalyzerSegments(
    segments.filter((segment) => segment.score >= ANALYZER_SCORE_CUTOFF)
  );
}

/** Alternatives retained by presentation after a group has won a path. */
export function selectAnalyzerAlternatives(
  segments: readonly AnalyzerSegment[]
): AnalyzerSegment[] {
  if (segments.length === 0) return [];
  const cutoff = segments[0]!.score * ANALYZER_SEGMENT_SCORE_CUTOFF;
  return segments.filter((segment) => segment.score >= cutoff);
}
