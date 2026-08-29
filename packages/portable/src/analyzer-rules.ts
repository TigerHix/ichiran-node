import {
  ANALYZER_SCORE_FLAG_COMMON,
  ANALYZER_SCORE_FLAG_LONG,
  ANALYZER_SCORE_FLAG_PRIMARY,
  ANALYZER_SCORE_FLAG_STRONG,
  type AnalyzerInitialResolver,
  type AnalyzerPathAdjustment,
  type AnalyzerPathTransition,
  type AnalyzerSegment,
  type AnalyzerSegmentGroup,
  type AnalyzerTransitionResolver
} from './analyzer-types.js';

type SegmentFilter = (segment: AnalyzerSegment) => boolean;
type GroupPair = readonly [AnalyzerSegmentGroup | null, AnalyzerSegmentGroup];
type Segfilter = (
  left: AnalyzerSegmentGroup | null,
  right: AnalyzerSegmentGroup
) => readonly GroupPair[];

const NOUN_PARTICLES = [
  2028920, 2028930, 2028990, 2028980, 2029000, 1007340, 1579080,
  1525680, 2028940, 1582300, 2215430, 1469800, 1009990, 2029010,
  1005120, 2034520, 1005120, 1008490, 1008530, 1008590, 2028950,
  2028960, 1009600
] as const;

const SEMI_FINAL_PARTICLES = [
  2017770, 2425930, 2130430, 2029130, 2834812, 2718360, 2201380,
  2722170, 2751630, 2029120, 2086640, 2029110, 2029080, 2029100
] as const;

function filteredGroup(
  group: AnalyzerSegmentGroup,
  segments: readonly AnalyzerSegment[]
): AnalyzerSegmentGroup {
  return { ...group, segments };
}

function classify(
  segments: readonly AnalyzerSegment[],
  filter: SegmentFilter
): readonly [AnalyzerSegment[], AnalyzerSegment[]] {
  const yes: AnalyzerSegment[] = [];
  const no: AnalyzerSegment[] = [];
  for (const segment of segments) (filter(segment) ? yes : no).push(segment);
  return [yes, no];
}

function hasSequence(segment: AnalyzerSegment, values: readonly number[]): boolean {
  const seqSet = segment.rules?.scoreInfo?.seqSet;
  return seqSet !== undefined && values.some((value) => seqSet.includes(value));
}

function isSimpleWithSequence(segment: AnalyzerSegment, values: readonly number[]): boolean {
  return segment.rules?.wordKind === 'simple' && hasSequence(segment, values);
}

function hasConjugationType(segment: AnalyzerSegment, type: number): boolean {
  return segment.rules?.scoreInfo?.conjugations.some(
    (conjugation) => conjugation.property.type === type
  ) ?? false;
}

function compoundEndsInSequence(segment: AnalyzerSegment, values: readonly number[]): boolean {
  const seq = segment.rules?.compoundEndSeq;
  return seq !== null && seq !== undefined && values.includes(seq);
}

function compoundEndsInText(segment: AnalyzerSegment, values: readonly string[]): boolean {
  const text = segment.rules?.compoundEndText;
  return text !== null && text !== undefined && values.includes(text);
}

function segmentText(segment: AnalyzerSegment): string {
  return segment.rules?.text ?? '';
}

function mustFollow(
  leftFilter: SegmentFilter,
  rightFilter: SegmentFilter,
  allowFirst = false
): Segfilter {
  return (left, right) => {
    const [rightYes, rightNo] = classify(right.segments, rightFilter);
    if (rightYes.length === 0 || (allowFirst && left === null)) return [[left, right]];

    if (left === null || left.end !== right.start) {
      return rightNo.length > 0 ? [[left, filteredGroup(right, rightNo)]] : [];
    }

    const [leftYes, leftNo] = classify(left.segments, leftFilter);
    if (leftNo.length === 0) return [[left, right]];

    const output: GroupPair[] = [];
    if (rightNo.length > 0) output.push([left, filteredGroup(right, rightNo)]);
    if (leftYes.length > 0) {
      output.push([
        filteredGroup(left, leftYes),
        filteredGroup(right, rightYes)
      ]);
    }
    return output;
  };
}

const SEGFILTERS: readonly Segfilter[] = [
  mustFollow(
    (segment) => hasConjugationType(segment, 13),
    (segment) => hasSequence(segment, [1342560])
  ),
  mustFollow(
    (segment) => !hasSequence(segment, [2221640]),
    (segment) => hasSequence(segment, [1577980]),
    true
  ),
  mustFollow(
    (segment) => !isSimpleWithSequence(segment, NOUN_PARTICLES),
    (segment) => hasSequence(segment, [2139720, 2849370, 2849387]),
    true
  ),
  mustFollow(
    (segment) => hasSequence(segment, [2029010]),
    (segment) => hasSequence(segment, [2087020])
  ),
  mustFollow(
    () => false,
    (segment) => compoundEndsInText(segment, ['ちゃい', 'いか', 'とか', 'とき', 'い'])
  ),
  mustFollow(
    () => false,
    (segment) => hasConjugationType(segment, 54) && segmentText(segment).endsWith('好き')
  ),
  mustFollow(
    (segment) => !compoundEndsInText(segment, ['いろ']),
    (segment) => segmentText(segment).startsWith('く'),
    true
  ),
  mustFollow(
    (segment) => !compoundEndsInSequence(segment, [2029120]),
    (segment) => segmentText(segment).startsWith('え'),
    true
  ),
  mustFollow(
    (segment) => !compoundEndsInSequence(segment, [2028920]),
    (segment) => hasSequence(segment, [1529520, 1296400, 2139720]),
    true
  ),
  mustFollow(
    (segment) => !hasSequence(segment, [1469800]),
    (segment) => hasSequence(segment, [1601080]),
    true
  ),
  mustFollow(
    (segment) => !hasSequence(segment, [2837117]),
    (segment) => hasSequence(segment, [1589350, 1587040]),
    true
  ),
  mustFollow(
    (segment) => !hasSequence(segment, [1008490]),
    (segment) => hasSequence(segment, [2086960]),
    true
  ),
  mustFollow(
    (segment) => !hasSequence(segment, [2089020]) || hasSequence(segment, [2028980]),
    (segment) => hasSequence(segment, [1157170, 2424740, 1305070]),
    true
  ),
  mustFollow(
    (segment) => !hasSequence(segment, [1896380, 2422860]),
    (segment) => hasSequence(segment, [2830009, 1547720]),
    true
  ),
  mustFollow(
    (segment) => !isSimpleWithSequence(segment, NOUN_PARTICLES),
    (segment) => hasSequence(segment, [1247260])
  )
];

/** Apply all analyzer-internal must-follow filters in current registration order. */
export function applyAnalyzerSegfilters(
  left: AnalyzerSegmentGroup | null,
  right: AnalyzerSegmentGroup
): GroupPair[] {
  let splits: GroupPair[] = [[left, right]];
  for (const segfilter of SEGFILTERS) {
    const next: GroupPair[] = [];
    for (const split of splits) next.push(...segfilter(split[0], split[1]));
    splits = next;
  }
  return splits;
}

function flag(segment: AnalyzerSegment, value: number): boolean {
  return ((segment.rules?.scoreInfo?.flags ?? 0) & value) !== 0;
}

function hasPosition(segment: AnalyzerSegment, positions: readonly string[]): boolean {
  const actual = segment.rules?.scoreInfo?.positions;
  return actual !== undefined && positions.some((position) => actual.includes(position));
}

function isNoun(segment: AnalyzerSegment): boolean {
  const nounShape = flag(segment, ANALYZER_SCORE_FLAG_LONG)
    || flag(segment, ANALYZER_SCORE_FLAG_STRONG)
    || (flag(segment, ANALYZER_SCORE_FLAG_PRIMARY) && flag(segment, ANALYZER_SCORE_FLAG_COMMON));
  if (nounShape && hasPosition(segment, ['n', 'n-adv', 'n-t', 'adj-na', 'n-suf', 'pn'])) {
    return true;
  }
  return segment.rules?.wordKind === 'counter'
    && (segment.rules.scoreInfo?.seqSet.length ?? 0) > 0;
}

function isPosition(
  segment: AnalyzerSegment,
  positions: readonly string[],
  shape: (strong: boolean, primary: boolean, common: boolean, long: boolean) => boolean
): boolean {
  if (!shape(
    flag(segment, ANALYZER_SCORE_FLAG_STRONG),
    flag(segment, ANALYZER_SCORE_FLAG_PRIMARY),
    flag(segment, ANALYZER_SCORE_FLAG_COMMON),
    flag(segment, ANALYZER_SCORE_FLAG_LONG)
  )) return false;
  return hasPosition(segment, positions);
}

interface SynergyRule {
  readonly left: SegmentFilter;
  readonly right: SegmentFilter;
  readonly description: string;
  readonly connector: string;
  readonly score: number | ((left: AnalyzerSegmentGroup, right: AnalyzerSegmentGroup) => number);
}

const SYNERGIES: readonly SynergyRule[] = [
  {
    left: isNoun,
    right: (segment) => hasSequence(segment, NOUN_PARTICLES),
    description: 'noun+prt', connector: ' ',
    score: (_left, right) => 10 + 4 * (right.end - right.start)
  },
  {
    left: isNoun, right: (segment) => hasSequence(segment, [2089020]),
    description: 'noun+da', connector: ' ', score: 10
  },
  {
    left: (segment) => hasSequence(segment, [1469800, 2139720]),
    right: (segment) => hasSequence(segment, [2089020, 1007370, 1928670]),
    description: 'no da/desu', connector: ' ', score: 15
  },
  {
    left: (segment) => hasSequence(segment, [2137720]),
    right: (segment) => hasSequence(segment, [2140410]),
    description: 'sou na n da', connector: ' ', score: 50
  },
  {
    left: (segment) => isPosition(
      segment, ['adj-no'], (strong, primary, common, long) => strong || long || (primary && common)
    ),
    right: (segment) => hasSequence(segment, [1469800]),
    description: 'no-adjective', connector: ' ', score: 15
  },
  {
    left: (segment) => isPosition(
      segment, ['adj-na'], (strong, primary, common, long) => strong || long || (primary && common)
    ),
    right: (segment) => hasSequence(segment, [2029110, 2028990]),
    description: 'na-adjective', connector: ' ', score: 15
  },
  {
    left: (segment) => isPosition(
      segment, ['adv-to'], (strong, primary, _common, long) => strong || long || primary
    ),
    right: (segment) => hasSequence(segment, [1008490]),
    description: 'to-adverb', connector: ' ',
    score: (left) => 10 + 10 * (left.end - left.start)
  },
  {
    left: isNoun, right: (segment) => hasSequence(segment, [1620400, 2083570]),
    description: 'suffix-chu', connector: '-', score: 12
  },
  {
    left: isNoun, right: (segment) => hasSequence(segment, [1416220]),
    description: 'suffix-tachi', connector: '-', score: 10
  },
  {
    left: isNoun, right: (segment) => hasSequence(segment, [1361140]),
    description: 'suffix-buri', connector: '', score: 40
  },
  {
    left: isNoun, right: (segment) => hasSequence(segment, [1375260]),
    description: 'suffix-sei', connector: '', score: 12
  },
  {
    left: (segment) => hasSequence(segment, [1270190]),
    right: (segment) => isPosition(
      segment, ['n'], (strong, _primary, _common, long) => strong || long
    ),
    description: 'o+noun', connector: '', score: 10
  },
  {
    left: (segment) => hasSequence(segment, [2242840, 1922780, 2423740]),
    right: (segment) => isPosition(segment, ['n'], (strong) => strong),
    description: 'kanji prefix+noun', connector: '', score: 15
  },
  {
    left: (segment) => compoundEndsInSequence(segment, [2028920]),
    right: (segment) => hasSequence(segment, [1000730, 1612750, 1409110, 2829697, 1587610]),
    description: 'shicha ikenai', connector: ' ', score: 50
  },
  {
    left: (segment) => hasSequence(segment, [1005460]),
    right: (segment) => segment.rules?.scoreInfo?.conjugations.some(
      (conjugation) => conjugation.property.negative !== false
    ) ?? false,
    description: 'shika+neg', connector: ' ', score: 50
  },
  {
    left: (segment) => hasSequence(segment, [1469800]),
    right: (segment) => hasSequence(segment, [1432920]),
    description: 'no toori', connector: ' ', score: 50
  },
  {
    left: (segment) => isPosition(segment, ['ctr'], () => true),
    right: (segment) => hasSequence(segment, [2854117, 2084550]),
    description: '', connector: '', score: 20
  }
];

function adjustment(
  left: AnalyzerSegmentGroup,
  right: AnalyzerSegmentGroup,
  description: string,
  connector: string,
  score: number
): AnalyzerPathAdjustment {
  return { start: left.end, end: right.start, description, connector, score };
}

function penalty(
  left: AnalyzerSegmentGroup,
  right: AnalyzerSegmentGroup
): AnalyzerPathAdjustment | undefined {
  // `pushnew` puts the later semi-final definition before the short penalty.
  if (
    left.end === right.start
    && left.segments.some((segment) => hasSequence(segment, SEMI_FINAL_PARTICLES))
  ) {
    return adjustment(left, right, 'semi-final not final', ' ', -15);
  }

  const short = (group: AnalyzerSegmentGroup, exceptTo: boolean): boolean => {
    const first = group.segments[0];
    return first !== undefined
      && group.end - group.start <= 1
      && !flag(first, ANALYZER_SCORE_FLAG_STRONG)
      && (!exceptTo || segmentText(first) !== 'と');
  };
  return short(left, false) && short(right, true)
    ? adjustment(left, right, 'short', ' ', -9)
    : undefined;
}

function synergyTransitions(
  left: AnalyzerSegmentGroup,
  right: AnalyzerSegmentGroup
): AnalyzerPathTransition[] {
  if (left.end !== right.start) return [];
  const output: AnalyzerPathTransition[] = [];
  for (const rule of SYNERGIES) {
    const leftSegments = left.segments.filter(rule.left);
    const rightSegments = right.segments.filter(rule.right);
    if (leftSegments.length === 0 || rightSegments.length === 0) continue;
    const score = typeof rule.score === 'number' ? rule.score : rule.score(left, right);
    output.push({
      right: filteredGroup(right, rightSegments),
      adjustment: adjustment(left, right, rule.description, rule.connector, score),
      left: filteredGroup(left, leftSegments)
    });
  }
  return output;
}

/** Current analyzer-internal filtering for a group at the start of a path. */
export const resolveAnalyzerInitialRules: AnalyzerInitialResolver = (group) =>
  applyAnalyzerSegfilters(null, group).map((split) => split[1]);

/** Current ordered baseline penalty + synergy transitions for one group pair. */
export const resolveAnalyzerRuleTransitions: AnalyzerTransitionResolver = (left, right) => {
  const output: AnalyzerPathTransition[] = [];
  for (const split of applyAnalyzerSegfilters(left, right)) {
    const filteredLeft = split[0];
    if (filteredLeft === null) continue;
    output.push({
      right: split[1],
      adjustment: penalty(filteredLeft, split[1]),
      left: filteredLeft
    });
    output.push(...synergyTransitions(filteredLeft, split[1]));
  }
  return output;
};

/** Ready-to-pass resolvers for `findAnalyzerPaths`. */
export const ANALYZER_INTERNAL_PATH_RULES = Object.freeze({
  initial: resolveAnalyzerInitialRules,
  transition: resolveAnalyzerRuleTransitions
});
