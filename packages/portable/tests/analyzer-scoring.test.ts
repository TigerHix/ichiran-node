import { describe, expect, test } from 'bun:test';
import {
  cullSegments as currentCullSegments,
  compareCommon as currentCompareCommon,
  gapPenalty as currentGapPenalty
} from '../../core/src/dict/scoring.js';
import {
  ANALYZER_SCORE_CUTOFF,
  compareAnalyzerCommon,
  cullAnalyzerSegments,
  filterAndCullAnalyzerSegments,
  scoreAnalyzerCandidate,
  selectAnalyzerAlternatives
} from '../src/analyzer-scoring.js';
import { analyzerGapPenalty } from '../src/analyzer-paths.js';
import {
  ANALYZER_SCORE_FLAG_COMMON,
  ANALYZER_SCORE_FLAG_LONG,
  ANALYZER_SCORE_FLAG_PRIMARY,
  ANALYZER_SCORE_FLAG_STRONG,
  type AnalyzerConjugation,
  type AnalyzerSegment,
  type AnalyzerSequenceFacts,
  type AnalyzerWordScoreFacts
} from '../src/analyzer-types.js';

const ACTIVE_SEQUENCE: AnalyzerSequenceFacts = {
  allArchived: false,
  preferKana: false,
  preferKanaOnOrdinalZero: false
};

function word(
  overrides: Partial<AnalyzerWordScoreFacts> = {}
): AnalyzerWordScoreFacts {
  const text = overrides.text ?? 'かな';
  return {
    kind: 'word',
    text,
    trueText: text,
    trueTextFollowsText: true,
    route: 'kana',
    seq: 1_000_001,
    ord: 0,
    common: null,
    nokanji: false,
    entry: { root: true, nKanji: 0, primaryNokanji: false },
    conjugationOnly: false,
    conjugations: [],
    positions: ['n'],
    self: ACTIVE_SEQUENCE,
    lineage: ACTIVE_SEQUENCE,
    inheritedCommon: null,
    inheritedOrd: null,
    split: null,
    suruBreak: null,
    ...overrides
  };
}

function conjugation(
  overrides: Partial<AnalyzerConjugation> = {}
): AnalyzerConjugation {
  return {
    seq: 2_000_001,
    from: 1_000_001,
    via: null,
    property: {
      pos: 'v1',
      type: 1,
      negative: false,
      formal: false
    },
    ...overrides
  };
}

function segment(
  candidateId: number,
  score: number,
  common: number | null
): AnalyzerSegment {
  return {
    candidateId,
    start: 0,
    end: 1,
    score,
    common,
    entity: false
  };
}

describe('portable analyzer scoring', () => {
  test('locks representative root, weak conjugation, particle, and skip scores', () => {
    const strongRoot = scoreAnalyzerCandidate(word({
      text: '日本語',
      trueText: '日本語',
      route: 'kanji',
      common: 1,
      entry: { root: true, nKanji: 1, primaryNokanji: false }
    }));
    expect(strongRoot.score).toBe(1088);
    expect(strongRoot.info.breakdown).toEqual({
      propertyScore: 32,
      kanjiBreak: null,
      useLengthBonus: 0,
      split: null
    });
    expect(strongRoot.info.flags).toBe(
      ANALYZER_SCORE_FLAG_STRONG
      | ANALYZER_SCORE_FLAG_PRIMARY
      | ANALYZER_SCORE_FLAG_COMMON
      | ANALYZER_SCORE_FLAG_LONG
    );

    const weakStem = scoreAnalyzerCandidate(word({
      text: 'たべ',
      trueText: 'たべ',
      seq: 2_000_001,
      ord: 3,
      entry: { root: false, nKanji: 1, primaryNokanji: false },
      conjugationOnly: true,
      conjugations: [conjugation({
        property: { pos: 'adj-i', type: 51, negative: false, formal: false }
      })],
      positions: ['adj-i'],
      inheritedCommon: 1,
      inheritedOrd: 0
    }));
    expect(weakStem.score).toBe(4);
    expect(weakStem.info.common).toBe(1);
    expect(weakStem.info.flags).toBe(ANALYZER_SCORE_FLAG_COMMON);

    const particle = word({
      text: 'ね',
      trueText: 'ね',
      seq: 2_029_080,
      common: 0,
      positions: ['prt']
    });
    expect(scoreAnalyzerCandidate(particle).score).toBe(6);
    expect(scoreAnalyzerCandidate(particle, { final: true }).score).toBe(16);

    const finalOnly = word({ text: 'かい', trueText: 'かい', seq: 2_017_770 });
    expect(scoreAnalyzerCandidate(finalOnly).score).toBe(0);
    expect(scoreAnalyzerCandidate(finalOnly, { final: true }).score).toBeGreaterThan(0);
  });

  test('preserves compound modifier and split arithmetic', () => {
    const base = word({
      text: 'テスト',
      trueText: 'テスト',
      common: 1
    });
    expect(scoreAnalyzerCandidate(base).score).toBe(720);

    const compound = scoreAnalyzerCandidate({
      kind: 'compound',
      text: 'テストです',
      base,
      modifier: { multiplier: 2, constant: 7 },
      conjugations: [],
      suruBreak: null
    });
    expect(compound.score).toBe(1117);
    expect(compound.info.breakdown.useLengthBonus).toBe(397);

    const additive = scoreAnalyzerCandidate(word({
      text: '日本語',
      trueText: '日本語',
      route: 'kanji',
      common: 1,
      entry: { root: true, nKanji: 1, primaryNokanji: false },
      split: { kind: 'add', score: -88 }
    }));
    expect(additive.score).toBe(1000);
    expect(additive.info.breakdown.split).toBe(-88);

    const proportional = scoreAnalyzerCandidate(word({
      text: '日本語',
      trueText: '日本語',
      route: 'kanji',
      common: 1,
      entry: { root: true, nKanji: 1, primaryNokanji: false },
      split: { kind: 'proportional', score: -2 }
    }));
    expect(proportional.score).toBe(1020);
    expect(proportional.info.breakdown.propertyScore).toBe(30);
    expect(proportional.info.breakdown.split).toBeNull();

    const parts = scoreAnalyzerCandidate(word({
      text: 'かなかな',
      trueText: 'かなかな',
      split: {
        kind: 'parts',
        score: 5,
        parts: [word({ text: 'かな', trueText: 'かな' }), word({ text: 'かな', trueText: 'かな' })]
      }
    }));
    expect(parts.info.breakdown.split).toEqual([5, 16, 16]);
    expect(parts.score).toBe(37);
  });

  test('preserves kanji-break exemptions, endpoint bonuses, and cutoff floor', () => {
    const ordinary = word({
      text: '日本語',
      trueText: '日本語',
      route: 'kanji',
      common: 1,
      entry: { root: true, nKanji: 1, primaryNokanji: false }
    });
    expect(scoreAnalyzerCandidate(ordinary, { kanjiBreak: [1] }).score).toBe(544);

    const prefix = word({
      text: '日本',
      trueText: '日本',
      route: 'kanji',
      positions: ['pref']
    });
    const rawPrefix = scoreAnalyzerCandidate(prefix).score;
    expect(scoreAnalyzerCandidate(prefix, { kanjiBreak: [1] }).score).toBe(
      Math.max(ANALYZER_SCORE_CUTOFF, Math.ceil(rawPrefix / 2) + 12)
    );

    const exempt = word({
      text: '飲む',
      trueText: '飲む',
      route: 'kanji',
      seq: 1_169_870,
      entry: { root: true, nKanji: 1, primaryNokanji: false }
    });
    expect(scoreAnalyzerCandidate(exempt, { kanjiBreak: [1] }).score).toBe(
      scoreAnalyzerCandidate(exempt).score
    );

    const suruSuffix = word({ text: 'する', trueText: 'する', positions: ['vs-i'] });
    const suruCompound = word({
      text: '日本語する',
      trueText: '日本語する',
      route: 'kanji',
      common: 1,
      positions: ['vs-s'],
      entry: { root: true, nKanji: 1, primaryNokanji: false },
      suruBreak: { suffixText: 'する', candidate: suruSuffix }
    });
    expect(scoreAnalyzerCandidate(suruCompound, { kanjiBreak: [1] }).score).toBe(
      scoreAnalyzerCandidate(suruSuffix).score + 50
    );
  });

  test('uses the dedicated counter root/POS floor while retaining ordinary arithmetic', () => {
    const counter = scoreAnalyzerCandidate(word({
      kind: 'counter',
      text: '三人',
      trueText: '三人',
      route: 'kanji',
      common: 1,
      positions: ['n'],
      entry: { root: false, nKanji: 1, primaryNokanji: false }
    }));
    expect(counter.score).toBe(325);
    expect(counter.info.positions).toEqual(['ctr']);
  });
});

describe('portable candidate culling', () => {
  test('differentially matches current stable common/score ordering', () => {
    const commonValues = [null, 0, 1, 2, 9] as const;
    for (const left of commonValues) {
      for (const right of commonValues) {
        expect(compareAnalyzerCommon(left, right)).toBe(currentCompareCommon(left, right));
      }
    }

    const input = [
      segment(0, 30, null),
      segment(1, 30, 0),
      segment(2, 30, 2),
      segment(3, 30, 1),
      segment(4, 15, 1),
      segment(5, 14, 1),
      segment(6, 31, null)
    ];
    const current = currentCullSegments(input.map((item) => ({
      start: item.start,
      end: item.end,
      word: {} as never,
      score: item.score,
      info: {
        common: item.common,
        posi: [], seqSet: [], conj: [], scoreInfo: [0, null, 0, null],
        kpcl: [false, false, false, false]
      }
    })) as never[]);
    const expectedOrder = current.map((item) =>
      input.find((candidate) => candidate.score === item.score && candidate.common === item.info?.common)!.candidateId
    );
    expect(cullAnalyzerSegments(input).map((item) => item.candidateId)).toEqual(expectedOrder);
  });

  test('applies lookup and presentation cutoffs in their legacy order', () => {
    const input = [segment(0, 10, null), segment(1, 6.7, null), segment(2, 5, null), segment(3, 4, null)];
    expect(filterAndCullAnalyzerSegments(input).map((item) => item.candidateId)).toEqual([0, 1, 2]);
    expect(selectAnalyzerAlternatives(input).map((item) => item.candidateId)).toEqual([0, 1]);
    for (let start = 0; start < 5; start++) {
      for (let end = start; end < 8; end++) {
        expect(analyzerGapPenalty(start, end)).toBe(currentGapPenalty(start, end));
      }
    }
  });
});
