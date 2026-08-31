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
  analyzerShortPenaltyLeft,
  resolveAnalyzerInitialRules,
  resolveAnalyzerRuleTransitions
} from './analyzer-rules.js';
import {
  validateAnalyzerEntities,
  validateAnalyzerLimit
} from './analyzer-options.js';

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
  readonly path: PathNode | null;
  readonly order?: EventOrder;
}

/** Original source-major registration order used to break equal-score ties. */
interface EventOrder {
  readonly sourceIndex: number;
  /** -1 is the source group's initial candidate, before its outgoing edges. */
  readonly destinationIndex: number;
  readonly priorRank: number;
  readonly splitRank: number;
}

/** Newest-first persistent path. Transitions replace only the newest group. */
interface PathNode {
  readonly part: AnalyzerPathPart;
  readonly next: PathNode | null;
}

/** Fixed-size insertion buffer with the current analyzer's stable tie behavior. */
class TopItems {
  readonly #items: Array<TopItem | null>;
  #count = 0;

  constructor(limit: number) {
    this.#items = new Array(limit).fill(null);
  }

  register(score: number, path: PathNode | null, order?: EventOrder): void {
    const item: TopItem = order === undefined ? { score, path } : { score, path, order };
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

interface PriorCandidate {
  readonly sourceIndex: number;
  readonly priorRank: number;
  readonly prior: TopItem;
  readonly head: AnalyzerSegmentGroup;
  readonly leftScore: number;
  readonly shortLeft: boolean;
}

interface MetricItem {
  readonly key: number;
  readonly candidate: PriorCandidate;
}

/** Top-N eligible predecessors for one affine non-adjacent score branch. */
class MetricFrontier {
  readonly #limit: number;
  readonly #items: MetricItem[] = [];

  constructor(limit: number) {
    this.#limit = limit;
  }

  register(candidate: PriorCandidate, key: number): void {
    this.#items.push({ key, candidate });
    this.#items.sort((left, right) =>
      right.key - left.key
      || left.candidate.sourceIndex - right.candidate.sourceIndex
      || left.candidate.priorRank - right.candidate.priorRank);
    if (this.#items.length > this.#limit) this.#items.pop();
  }

  values(): readonly PriorCandidate[] {
    return this.#items.map(item => item.candidate);
  }
}

function compareEventOrder(left: EventOrder, right: EventOrder): number {
  return left.sourceIndex - right.sourceIndex
    || left.destinationIndex - right.destinationIndex
    || left.priorRank - right.priorRank
    || left.splitRank - right.splitRank;
}

function materializePath(path: PathNode | null): AnalyzerPathPart[] {
  const newestFirst: AnalyzerPathPart[] = [];
  for (let node = path; node !== null; node = node.next) newestFirst.push(node.part);
  newestFirst.reverse();
  return newestFirst;
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

function transitionPath(
  prior: TopItem,
  split: ReturnType<AnalyzerTransitionResolver>[number]
): PathNode {
  const replacedLeft: PathNode = {
    part: split.left,
    next: prior.path?.next ?? null
  };
  const adjustment: PathNode = split.adjustment
    ? { part: split.adjustment, next: replacedLeft }
    : replacedLeft;
  return { part: split.right, next: adjustment };
}

/**
 * Exact default-rule DP with a sweep for non-adjacent predecessors.
 *
 * Across a gap the built-in rules never filter the left group or emit a
 * synergy. Their score is the maximum of two affine predecessor keys, with
 * the short-word penalty selecting one of two left categories. Therefore an
 * exact destination top-N can only come from the union of three size-N
 * frontiers. Adjacent pairs still run the complete rule resolver.
 */
function findDefaultAnalyzerPaths(
  groups: readonly AnalyzerSegmentGroup[],
  textLength: number,
  limit: number,
  entities: readonly AnalyzerEntityHint[],
  initial: AnalyzerInitialResolver
): AnalyzerPathResult[] {
  const groupTops = groups.map(() => new TopItems(limit));
  const candidatesByGroup: PriorCandidate[][] = groups.map(() => []);
  const indexesByEnd = new Map<number, number[]>();
  for (let index = 0; index < groups.length; index++) {
    const end = groups[index]!.end;
    const values = indexesByEnd.get(end);
    if (values === undefined) indexesByEnd.set(end, [index]);
    else values.push(index);
  }
  const activationOrder = groups.map((_, index) => index).sort((left, right) =>
    groups[left]!.end - groups[right]!.end || left - right);
  let activationIndex = 0;
  const normalScore = new MetricFrontier(limit);
  const shortScore = new MetricFrontier(limit);
  const scoreWithoutLeft = new MetricFrontier(limit);

  for (let rightIndex = 0; rightIndex < groups.length; rightIndex++) {
    const second = groups[rightIndex]!;
    while (
      activationIndex < activationOrder.length
      && groups[activationOrder[activationIndex]!]!.end < second.start
    ) {
      const sourceIndex = activationOrder[activationIndex++]!;
      for (const candidate of candidatesByGroup[sourceIndex]!) {
        const gapCredit = -ANALYZER_GAP_PENALTY * candidate.head.end;
        (candidate.shortLeft ? shortScore : normalScore).register(
          candidate,
          candidate.prior.score + gapCredit
        );
        scoreWithoutLeft.register(
          candidate,
          candidate.prior.score + gapCredit - candidate.leftScore
        );
      }
    }

    const incoming = new Set<PriorCandidate>();
    for (const candidate of normalScore.values()) incoming.add(candidate);
    for (const candidate of shortScore.values()) incoming.add(candidate);
    for (const candidate of scoreWithoutLeft.values()) incoming.add(candidate);
    for (const sourceIndex of indexesByEnd.get(second.start) ?? []) {
      if (sourceIndex >= rightIndex) break;
      for (const candidate of candidatesByGroup[sourceIndex]!) incoming.add(candidate);
    }
    const orderedIncoming = [...incoming].sort((left, right) =>
      left.sourceIndex - right.sourceIndex || left.priorRank - right.priorRank);

    const secondTop = groupTops[rightIndex]!;
    const secondScore = groupScore(second);
    const secondEntityBoost = entityBoost(second, entities);
    const resolvedByHead = new Map<
      AnalyzerSegmentGroup,
      ReturnType<AnalyzerTransitionResolver>
    >();
    for (const candidate of orderedIncoming) {
      let resolved = resolvedByHead.get(candidate.head);
      if (resolved === undefined) {
        resolved = resolveAnalyzerRuleTransitions(candidate.head, second);
        resolvedByHead.set(candidate.head, resolved);
      }
      const pairGap = analyzerGapPenalty(candidate.head.end, second.start);
      const scoreTail = candidate.prior.score - candidate.leftScore;
      for (let splitRank = 0; splitRank < resolved.length; splitRank++) {
        const split = resolved[splitRank]!;
        const splitScore = groupScore(split.right)
          + (split.adjustment?.score ?? 0)
          + groupScore(split.left)
          + secondEntityBoost;
        const accumulated = pairGap
          + Math.max(splitScore, candidate.leftScore + 1, secondScore + 1)
          + scoreTail;
        secondTop.register(
          accumulated,
          transitionPath(candidate.prior, split),
          {
            sourceIndex: candidate.sourceIndex,
            destinationIndex: rightIndex,
            priorRank: candidate.priorRank,
            splitRank
          }
        );
      }
    }

    const gapLeft = analyzerGapPenalty(0, second.start);
    let initialRank = 0;
    for (const filtered of initial(second)) {
      const score = groupScore(filtered) + secondEntityBoost;
      secondTop.register(
        gapLeft + score,
        { part: filtered, next: null },
        {
          sourceIndex: rightIndex,
          destinationIndex: -1,
          priorRank: initialRank++,
          splitRank: 0
        }
      );
    }

    candidatesByGroup[rightIndex] = secondTop.values().map((prior, priorRank) => {
      const head = prior.path?.part;
      if (!head || !isSegmentGroup(head)) {
        throw new Error('Analyzer path has no final segment group');
      }
      return {
        sourceIndex: rightIndex,
        priorRank,
        prior,
        head,
        leftScore: groupScore(head),
        shortLeft: analyzerShortPenaltyLeft(head)
      };
    });
  }

  const completed: TopItem[] = [{
    score: analyzerGapPenalty(0, textLength),
    path: null,
    order: { sourceIndex: -1, destinationIndex: -1, priorRank: 0, splitRank: 0 }
  }];
  for (let index = 0; index < groups.length; index++) {
    const finalGap = analyzerGapPenalty(groups[index]!.end, textLength);
    for (const item of groupTops[index]!.values()) {
      if (item.order === undefined) throw new Error('Analyzer path has no event order');
      completed.push({ score: item.score + finalGap, path: item.path, order: item.order });
    }
  }
  completed.sort((left, right) =>
    right.score - left.score || compareEventOrder(left.order!, right.order!));
  return completed.slice(0, limit).map(item => ({
    score: item.score,
    parts: materializePath(item.path)
  }));
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
  if (!Number.isSafeInteger(textLength) || textLength < 0) {
    throw new TypeError('textLength must be a non-negative safe integer');
  }
  const limit = validateAnalyzerLimit(options.limit);
  const entities = validateAnalyzerEntities(textLength, options.entities);
  const groups = preparedGroups(inputGroups);
  const initial = options.initial ?? resolveAnalyzerInitialRules;
  const transition = options.transition ?? resolveAnalyzerRuleTransitions;
  if (
    initial === resolveAnalyzerInitialRules
    && transition === resolveAnalyzerRuleTransitions
  ) {
    return findDefaultAnalyzerPaths(groups, textLength, limit, entities, initial);
  }
  const top = new TopItems(limit);
  const groupTops = groups.map(() => new TopItems(limit));

  top.register(analyzerGapPenalty(0, textLength), null);

  for (let leftIndex = 0; leftIndex < groups.length; leftIndex++) {
    const first = groups[leftIndex]!;
    const firstTop = groupTops[leftIndex]!;
    const gapLeft = analyzerGapPenalty(0, first.start);
    const gapRight = analyzerGapPenalty(first.end, textLength);
    const firstEntityBoost = entityBoost(first, entities);

    for (const filtered of initial(first)) {
      const score = groupScore(filtered) + firstEntityBoost;
      const path = { part: filtered, next: null };
      firstTop.register(gapLeft + score, path);
      top.register(gapLeft + score + gapRight, path);
    }

    const priorValues = firstTop.values();
    for (let rightIndex = leftIndex + 1; rightIndex < groups.length; rightIndex++) {
      const second = groups[rightIndex]!;
      if (second.start < first.end) continue;

      const secondTop = groupTops[rightIndex]!;
      const secondScore = groupScore(second);
      const pairGap = analyzerGapPenalty(first.end, second.start);
      const finalGap = analyzerGapPenalty(second.end, textLength);
      const secondEntityBoost = entityBoost(second, entities);

      for (const prior of priorValues) {
        const priorHead = prior.path?.part;
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
          const path = transitionPath(prior, split);

          secondTop.register(accumulated, path);
          top.register(accumulated + finalGap, path);
        }
      }
    }
  }

  return top.values().map((item) => ({
    score: item.score,
    parts: materializePath(item.path)
  }));
}
