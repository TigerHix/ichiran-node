import { describe, expect, test } from 'bun:test';
import { getPenalties as currentPenalties } from '../../core/src/grammar/penalties.js';
import { applySegfilters as currentSegfilters } from '../../core/src/grammar/segfilters.js';
import { getSynergies as currentSynergies } from '../../core/src/grammar/synergies.js';
import type { CalcScoreInfo, Segment, SegmentList } from '../../core/src/types.js';
import {
  applyAnalyzerSegfilters,
  resolveAnalyzerInitialRules,
  resolveAnalyzerRuleTransitions
} from '../src/analyzer-rules.js';
import {
  ANALYZER_SCORE_FLAG_COMMON,
  ANALYZER_SCORE_FLAG_LONG,
  ANALYZER_SCORE_FLAG_PRIMARY,
  ANALYZER_SCORE_FLAG_STRONG,
  type AnalyzerConjugation,
  type AnalyzerPathTransition,
  type AnalyzerRuleWordKind,
  type AnalyzerScoreInfo,
  type AnalyzerSegment,
  type AnalyzerSegmentGroup
} from '../src/analyzer-types.js';

interface RuleSpec {
  readonly id: number;
  readonly text: string;
  readonly wordKind?: AnalyzerRuleWordKind;
  readonly seqSet?: readonly number[];
  readonly positions?: readonly string[];
  readonly flags?: number;
  readonly conjugations?: readonly AnalyzerConjugation[];
  readonly compoundEndSeq?: number | null;
  readonly compoundEndText?: string | null;
  readonly score?: number;
}

function conjugation(type: number, negative: boolean | null = false): AnalyzerConjugation {
  return {
    seq: 9_000_001 + type,
    from: 8_000_001,
    via: null,
    property: { pos: 'v1', type, negative, formal: false }
  };
}

function scoreInfo(spec: RuleSpec): AnalyzerScoreInfo {
  return {
    positions: spec.positions ?? [],
    seqSet: spec.seqSet ?? [],
    conjugations: spec.conjugations ?? [],
    common: null,
    breakdown: { propertyScore: 1, kanjiBreak: null, useLengthBonus: 0, split: null },
    flags: spec.flags ?? ANALYZER_SCORE_FLAG_STRONG
  };
}

function portableSegment(spec: RuleSpec, start: number, end: number): AnalyzerSegment {
  return {
    candidateId: spec.id,
    start,
    end,
    score: spec.score ?? 20,
    common: null,
    entity: false,
    rules: {
      text: spec.text,
      wordKind: spec.wordKind ?? 'simple',
      scoreInfo: scoreInfo(spec),
      compoundEndSeq: spec.compoundEndSeq ?? null,
      compoundEndText: spec.compoundEndText ?? null
    }
  };
}

function coreWord(spec: RuleSpec): any {
  const simple = (seq: number, text: string) => ({
    id: spec.id,
    seq,
    text,
    ord: 0,
    common: null,
    commonTags: '',
    conjugateP: false,
    nokanji: false,
    bestKanji: null,
    __candidateId: spec.id
  });
  const seq = spec.seqSet?.[0] ?? 7_000_000 + spec.id;
  const kind = spec.wordKind ?? 'simple';
  if (kind === 'simple') return simple(seq, spec.text);
  if (kind === 'proxy') {
    return {
      source: simple(seq, spec.text),
      text: spec.text,
      kana: spec.text,
      __candidateId: spec.id
    };
  }
  if (kind === 'counter') {
    return {
      text: spec.text,
      kana: spec.text,
      valueString: () => spec.text,
      getText: () => spec.text,
      __candidateId: spec.id
    };
  }

  const primary = simple(seq, spec.text);
  const endText = spec.compoundEndText ?? '末';
  const last = spec.compoundEndSeq === null || spec.compoundEndSeq === undefined
    ? {
        source: simple(6_000_000 + spec.id, endText),
        text: endText,
        kana: endText,
        __candidateId: spec.id
      }
    : simple(spec.compoundEndSeq, endText);
  return {
    text: spec.text,
    kana: spec.text,
    primary,
    words: [primary, last],
    seq: [seq],
    scoreMod: 0,
    __candidateId: spec.id
  };
}

function coreInfo(spec: RuleSpec): CalcScoreInfo {
  const info = scoreInfo(spec);
  return {
    posi: [...info.positions],
    seqSet: [...info.seqSet],
    conj: info.conjugations.map((value, index) => ({
      seq: value.seq,
      from: value.from,
      via: value.via,
      prop: {
        id: index,
        conjId: index,
        conjType: value.property.type,
        pos: value.property.pos,
        neg: value.property.negative,
        fml: value.property.formal
      },
      srcMap: []
    })),
    common: null,
    scoreInfo: [1, null, 0, null],
    kpcl: [
      (info.flags & ANALYZER_SCORE_FLAG_STRONG) !== 0,
      (info.flags & ANALYZER_SCORE_FLAG_PRIMARY) !== 0,
      (info.flags & ANALYZER_SCORE_FLAG_COMMON) !== 0,
      (info.flags & ANALYZER_SCORE_FLAG_LONG) !== 0
    ]
  };
}

function portableGroup(
  groupId: number,
  start: number,
  end: number,
  specs: readonly RuleSpec[]
): AnalyzerSegmentGroup {
  return {
    groupId,
    start,
    end,
    matches: specs.length,
    segments: specs.map((spec) => portableSegment(spec, start, end))
  };
}

function coreGroup(start: number, end: number, specs: readonly RuleSpec[]): SegmentList {
  return {
    start,
    end,
    matches: specs.length,
    segments: specs.map((spec) => ({
      start,
      end,
      word: coreWord(spec),
      score: spec.score ?? 20,
      info: coreInfo(spec)
    }))
  };
}

function portableIds(group: AnalyzerSegmentGroup): number[] {
  return group.segments.map((segment) => segment.candidateId);
}

function coreIds(group: SegmentList): number[] {
  return group.segments.map((segment) => (segment.word as any).__candidateId);
}

function portableTransitionSignature(transition: AnalyzerPathTransition): unknown {
  return {
    right: portableIds(transition.right),
    adjustment: transition.adjustment ?? null,
    left: portableIds(transition.left)
  };
}

function coreTransitionSignature(parts: readonly any[]): unknown {
  return {
    right: coreIds(parts[0]),
    adjustment: parts.length === 3 ? parts[1] : null,
    left: coreIds(parts[parts.length - 1])
  };
}

async function currentTransitions(left: SegmentList, right: SegmentList): Promise<unknown[]> {
  const output: unknown[] = [];
  for (const [filteredLeft, filteredRight] of currentSegfilters(left, right)) {
    if (filteredLeft === null) continue;
    output.push(coreTransitionSignature(await currentPenalties(filteredLeft, filteredRight)));
    for (const synergy of await currentSynergies(filteredLeft, filteredRight)) {
      output.push(coreTransitionSignature(synergy));
    }
  }
  return output;
}

function expectPairParity(
  leftSpecs: readonly RuleSpec[],
  rightSpecs: readonly RuleSpec[],
  adjacent = true
): Promise<void> {
  const leftEnd = 2;
  const rightStart = adjacent ? leftEnd : leftEnd + 1;
  const portableLeft = portableGroup(1, 0, leftEnd, leftSpecs);
  const portableRight = portableGroup(2, rightStart, rightStart + 2, rightSpecs);
  const currentLeft = coreGroup(0, leftEnd, leftSpecs);
  const currentRight = coreGroup(rightStart, rightStart + 2, rightSpecs);
  return currentTransitions(currentLeft, currentRight).then((expected) => {
    expect(resolveAnalyzerRuleTransitions(portableLeft, portableRight).map(
      portableTransitionSignature
    )).toEqual(expected);

    const currentInitial = currentSegfilters(null, currentRight).map((split) => coreIds(split[1]));
    expect(resolveAnalyzerInitialRules(portableRight).map(portableIds)).toEqual(currentInitial);
  });
}

const NOUN_FLAGS = ANALYZER_SCORE_FLAG_STRONG | ANALYZER_SCORE_FLAG_PRIMARY;

describe('portable analyzer-internal path rules', () => {
  test('differentially matches every registered synergy', async () => {
    const noun = (id: number): RuleSpec => ({ id, text: `名${id}`, positions: ['n'], flags: NOUN_FLAGS });
    const cases: Array<readonly [string, RuleSpec, RuleSpec]> = [
      ['noun+prt', noun(1), { id: 2, text: 'は', seqSet: [2028920] }],
      ['noun+da', noun(3), { id: 4, text: 'だ', seqSet: [2089020] }],
      ['no da/desu', { id: 5, text: 'の', seqSet: [1469800] }, { id: 6, text: 'です', seqSet: [1007370] }],
      ['sou na n da', { id: 7, text: 'そう', seqSet: [2137720] }, { id: 8, text: 'なんだ', seqSet: [2140410] }],
      ['no-adjective', { id: 9, text: '特別', positions: ['adj-no'], flags: NOUN_FLAGS }, { id: 10, text: 'の', seqSet: [1469800] }],
      ['na-adjective', { id: 11, text: '静か', positions: ['adj-na'], flags: NOUN_FLAGS }, { id: 12, text: 'な', seqSet: [2029110] }],
      ['to-adverb', { id: 13, text: '堂々', positions: ['adv-to'], flags: ANALYZER_SCORE_FLAG_PRIMARY }, { id: 14, text: 'と', seqSet: [1008490] }],
      ['suffix-chu', noun(15), { id: 16, text: '中', seqSet: [1620400] }],
      ['suffix-tachi', noun(17), { id: 18, text: '達', seqSet: [1416220] }],
      ['suffix-buri', noun(19), { id: 20, text: '振り', seqSet: [1361140] }],
      ['suffix-sei', noun(21), { id: 22, text: '性', seqSet: [1375260] }],
      ['o+noun', { id: 23, text: 'お', seqSet: [1270190] }, noun(24)],
      ['kanji prefix+noun', { id: 25, text: '未', seqSet: [2242840] }, noun(26)],
      ['shicha ikenai', { id: 27, text: 'しちゃ', wordKind: 'compound', compoundEndSeq: 2028920, compoundEndText: 'は' }, { id: 28, text: 'いけない', seqSet: [1000730] }],
      ['shika+neg', { id: 29, text: 'しか', seqSet: [1005460] }, { id: 30, text: 'ない', conjugations: [conjugation(1, null)] }],
      ['no toori', { id: 31, text: 'の', seqSet: [1469800] }, { id: 32, text: '通り', seqSet: [1432920] }],
      ['', { id: 33, text: '三人', positions: ['ctr'], wordKind: 'counter', seqSet: [1] }, { id: 34, text: '置き', seqSet: [2854117] }]
    ];

    for (const [description, left, right] of cases) {
      await expectPairParity([left], [right]);
      const transitions = resolveAnalyzerRuleTransitions(
        portableGroup(1, 0, 2, [left]),
        portableGroup(2, 2, 4, [right])
      );
      expect(transitions.some((transition) =>
        transition.adjustment?.description === description && transition.adjustment.score > 0
      ), description || 'oki').toBe(true);
    }
  });

  test('differentially matches filtering, penalty priority, and non-adjacent behavior', async () => {
    const neutral: RuleSpec = { id: 100, text: '普通', positions: ['n'] };
    await expectPairParity(
      [{ id: 101, text: '連用', conjugations: [conjugation(13)] }, neutral],
      [{ id: 102, text: '始める', seqSet: [1342560] }, { ...neutral, id: 103 }]
    );
    await expectPairParity(
      [neutral],
      [{ id: 104, text: '始める', seqSet: [1342560] }, { ...neutral, id: 105 }],
      false
    );
    await expectPairParity(
      [{ id: 106, text: '語尾', wordKind: 'compound', compoundEndText: 'ちゃい' }, neutral],
      [{ id: 107, text: '語尾', wordKind: 'compound', compoundEndText: 'ちゃい' }, { ...neutral, id: 108 }]
    );
    await expectPairParity(
      [{ id: 109, text: 'かい', seqSet: [2017770], flags: 0 }],
      [{ id: 110, text: 'な', flags: 0 }]
    );
    await expectPairParity(
      [{ id: 111, text: 'あ', flags: 0 }],
      [{ id: 112, text: 'い', flags: 0 }],
      false
    );

    const shortSemiLeft = portableGroup(1, 0, 1, [{ id: 113, text: 'かい', seqSet: [2017770], flags: 0 }]);
    const shortRight = portableGroup(2, 1, 2, [{ id: 114, text: 'な', flags: 0 }]);
    expect(resolveAnalyzerRuleTransitions(shortSemiLeft, shortRight)[0]!.adjustment?.description).toBe(
      'semi-final not final'
    );
  });

  test('matches current rules across a deterministic mixed-feature corpus', async () => {
    const catalog: RuleSpec[] = [
      { id: 200, text: '普通', positions: ['n'] },
      { id: 201, text: '始める', seqSet: [1342560] },
      { id: 202, text: '連用', conjugations: [conjugation(13)] },
      { id: 203, text: 'いる', seqSet: [1577980] },
      { id: 204, text: 'つ', seqSet: [2221640] },
      { id: 205, text: 'ん', seqSet: [2139720] },
      { id: 206, text: 'は', seqSet: [2028920] },
      { id: 207, text: 'ちゃい', wordKind: 'compound', compoundEndText: 'ちゃい' },
      { id: 208, text: '大好き', conjugations: [conjugation(54)] },
      { id: 209, text: 'くる' },
      { id: 210, text: 'える' },
      { id: 211, text: 'さ', wordKind: 'compound', compoundEndSeq: 2029120, compoundEndText: 'さ' },
      { id: 212, text: '静か', positions: ['adj-na'], flags: NOUN_FLAGS },
      { id: 213, text: '三人', positions: ['ctr'], wordKind: 'counter', seqSet: [1] },
      { id: 214, text: 'しか', seqSet: [1005460] },
      { id: 215, text: 'ない', conjugations: [conjugation(1, null)] },
      { id: 216, text: 'あ', flags: 0 },
      { id: 217, text: 'と', flags: 0, seqSet: [1008490] },
      { id: 218, text: '君', seqSet: [1247260] },
      { id: 219, text: 'だ', seqSet: [2089020] }
    ];
    let state = 0x1234abcd;
    const random = (): number => {
      state = (Math.imul(state, 1664525) + 1013904223) >>> 0;
      return state;
    };
    for (let iteration = 0; iteration < 250; iteration++) {
      const left: RuleSpec[] = [];
      const right: RuleSpec[] = [];
      for (let index = 0; index < 1 + random() % 3; index++) {
        left.push(catalog[random() % catalog.length]!);
      }
      for (let index = 0; index < 1 + random() % 3; index++) {
        right.push(catalog[random() % catalog.length]!);
      }
      await expectPairParity(left, right, random() % 3 !== 0);
    }

    // Also compare the lower-level split representation on a multi-alternative case.
    const left = portableGroup(1, 0, 2, [catalog[2]!, catalog[0]!]);
    const right = portableGroup(2, 2, 4, [catalog[1]!, catalog[0]!]);
    const portable = applyAnalyzerSegfilters(left, right).map(([l, r]) => ({
      left: l ? portableIds(l) : null,
      right: portableIds(r)
    }));
    const current = currentSegfilters(
      coreGroup(0, 2, [catalog[2]!, catalog[0]!]),
      coreGroup(2, 4, [catalog[1]!, catalog[0]!])
    ).map(([l, r]) => ({ left: l ? coreIds(l) : null, right: coreIds(r) }));
    expect(portable).toEqual(current);
  });
});
