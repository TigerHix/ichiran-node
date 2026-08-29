import type {
  AnalyzerEntityHint,
  AnalyzerInitialResolver,
  AnalyzerPathPart,
  AnalyzerPathResult,
  AnalyzerSegment,
  AnalyzerSegmentGroup,
  AnalyzerTransitionResolver
} from './analyzer-types.js';
import {
  resolveAnalyzerInitialRules,
  resolveAnalyzerRuleTransitions
} from './analyzer-rules.js';

export const ANALYZER_GAP_PENALTY = -500;
export const DEFAULT_ANALYZER_ENTITY_BOOST = 50;

export interface AnalyzerPathOptions {
  readonly limit?: number;
  readonly entities?: readonly AnalyzerEntityHint[];
  /** Override the built-in analyzer-internal initial filters. */
  readonly initial?: AnalyzerInitialResolver;
  /** Override the built-in analyzer-internal pair rules. */
  readonly transition?: AnalyzerTransitionResolver;
}

interface TopItem {
  readonly score: number;
  readonly payload: readonly AnalyzerPathPart[];
}

/** Fixed-size insertion buffer with the current analyzer's stable tie behavior. */
class TopItems {
  readonly #items: Array<TopItem | null>;
  #count = 0;

  constructor(limit: number) {
    this.#items = new Array(limit).fill(null);
  }

  register(score: number, payload: readonly AnalyzerPathPart[]): void {
    const item: TopItem = { score, payload };
    const length = this.#items.length;
    for (let index = Math.min(this.#count, length); index >= 0; index--) {
      const previous = index > 0 ? this.#items[index - 1] : null;
      const done = previous === null || previous.score >= score;
      if (index < length) this.#items[index] = done ? item : previous;
      if (done) break;
    }
    this.#count++;
  }

  values(): TopItem[] {
    const result: TopItem[] = [];
    const length = Math.min(this.#count, this.#items.length);
    for (let index = 0; index < length; index++) {
      const item = this.#items[index];
      if (item !== null) result.push(item);
    }
    return result;
  }
}

export function analyzerGapPenalty(start: number, end: number): number {
  return (end - start) * ANALYZER_GAP_PENALTY;
}

function groupScore(group: AnalyzerSegmentGroup): number {
  return group.segments[0]?.score ?? 0;
}

function isSegmentGroup(part: AnalyzerPathPart): part is AnalyzerSegmentGroup {
  return 'segments' in part;
}

function entityBoost(
  group: AnalyzerSegmentGroup,
  entities: readonly AnalyzerEntityHint[]
): number {
  let boost = 0;
  for (const entity of entities) {
    if (entity.start === group.start && entity.end === group.end) {
      boost += entity.boost ?? DEFAULT_ANALYZER_ENTITY_BOOST;
    }
  }
  return boost;
}

function preparedGroups(groups: readonly AnalyzerSegmentGroup[]): AnalyzerSegmentGroup[] {
  const result = groups.map((group) => ({
    ...group,
    // Current expand-segment-list uses a stable descending score sort.
    segments: [...group.segments].sort((left, right) => right.score - left.score)
  }));
  // Entity groups are appended before this stable ordering in the current analyzer.
  result.sort((left, right) => left.start - right.start || left.end - right.end);
  return result;
}

/**
 * Add the synthetic entity candidates used by current segmentation.
 *
 * The path scorer applies the same hint again to every exactly matching span;
 * consequently a synthetic entity receives twice its configured boost. That
 * seemingly odd behavior is intentional parity, not a corrected formula.
 */
export function addAnalyzerEntityGroups(
  groups: readonly AnalyzerSegmentGroup[],
  entities: readonly AnalyzerEntityHint[],
  inputText = ''
): AnalyzerSegmentGroup[] {
  let groupId = groups.reduce((minimum, group) => Math.min(minimum, group.groupId), 0) - 1;
  let candidateId = -1;
  const result = [...groups];
  for (const entity of entities) {
    const boost = entity.boost ?? DEFAULT_ANALYZER_ENTITY_BOOST;
    const segment: AnalyzerSegment = {
      candidateId: candidateId--,
      start: entity.start,
      end: entity.end,
      score: boost,
      common: null,
      entity: true,
      rules: {
        text: inputText.slice(entity.start, entity.end),
        wordKind: 'simple',
        scoreInfo: null,
        compoundEndSeq: null,
        compoundEndText: null
      }
    };
    result.push({
      groupId: groupId--,
      start: entity.start,
      end: entity.end,
      segments: [segment],
      matches: 1
    });
  }
  return preparedGroups(result);
}

/**
 * Exact top-N/gap dynamic program over scored groups.
 *
 * Lookup owns candidate creation. Optional resolvers only materialize the
 * current pairwise filtering/adjustment outputs; this module owns ordering,
 * accumulation, gaps, entity boosts, top-N truncation, and tie stability.
 */
export function findAnalyzerPaths(
  inputGroups: readonly AnalyzerSegmentGroup[],
  textLength: number,
  options: AnalyzerPathOptions = {}
): AnalyzerPathResult[] {
  const limit = options.limit ?? 5;
  const entities = options.entities ?? [];
  const groups = preparedGroups(inputGroups);
  const top = new TopItems(limit);
  const groupTops = groups.map(() => new TopItems(limit));
  const initial = options.initial ?? resolveAnalyzerInitialRules;
  const transition = options.transition ?? resolveAnalyzerRuleTransitions;

  top.register(analyzerGapPenalty(0, textLength), []);

  for (let leftIndex = 0; leftIndex < groups.length; leftIndex++) {
    const first = groups[leftIndex]!;
    const firstTop = groupTops[leftIndex]!;
    const gapLeft = analyzerGapPenalty(0, first.start);
    const gapRight = analyzerGapPenalty(first.end, textLength);
    const firstEntityBoost = entityBoost(first, entities);

    for (const filtered of initial(first)) {
      const score = groupScore(filtered) + firstEntityBoost;
      firstTop.register(gapLeft + score, [filtered]);
      top.register(gapLeft + score + gapRight, [filtered]);
    }

    for (let rightIndex = leftIndex + 1; rightIndex < groups.length; rightIndex++) {
      const second = groups[rightIndex]!;
      if (second.start < first.end) continue;

      const secondTop = groupTops[rightIndex]!;
      const secondScore = groupScore(second);
      const pairGap = analyzerGapPenalty(first.end, second.start);
      const finalGap = analyzerGapPenalty(second.end, textLength);
      const secondEntityBoost = entityBoost(second, entities);

      for (const prior of firstTop.values()) {
        const priorHead = prior.payload[0];
        if (!priorHead || !isSegmentGroup(priorHead)) continue;
        const leftScore = groupScore(priorHead);
        const scoreTail = prior.score - leftScore;

        for (const split of transition(priorHead, second)) {
          const adjustmentScore = split.adjustment?.score ?? 0;
          const splitScore = groupScore(split.right)
            + adjustmentScore
            + groupScore(split.left)
            + secondEntityBoost;
          const accumulated = pairGap
            + Math.max(splitScore, leftScore + 1, secondScore + 1)
            + scoreTail;
          const payload: AnalyzerPathPart[] = [split.right];
          if (split.adjustment) payload.push(split.adjustment);
          payload.push(split.left, ...prior.payload.slice(1));

          secondTop.register(accumulated, payload);
          top.register(accumulated + finalGap, payload);
        }
      }
    }
  }

  return top.values().map((item) => ({
    score: item.score,
    parts: [...item.payload].reverse()
  }));
}
