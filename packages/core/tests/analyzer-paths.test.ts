import { describe, expect, test } from 'bun:test';
import { findBestPath as currentFindBestPath } from '../../reference-postgres/src/dict/segmentation.js';
import type { CalcScoreInfo, CompoundText, SegmentList } from '../../reference-postgres/src/types.js';
import {
  addAnalyzerEntityGroups,
  findAnalyzerPaths
} from '../src/analyzer-paths.js';
import { resolveAnalyzerRuleTransitions } from '../src/analyzer-rules.js';
import type {
  AnalyzerEntityHint,
  AnalyzerPathPart,
  AnalyzerSegmentGroup
} from '../src/analyzer-types.js';
import { ANALYZER_SCORE_FLAG_STRONG } from '../src/analyzer-types.js';

function portableGroup(
  groupId: number,
  start: number,
  end: number,
  scores: readonly number[]
): AnalyzerSegmentGroup {
  return {
    groupId,
    start,
    end,
    matches: scores.length,
    segments: scores.map((score, index) => ({
      candidateId: groupId * 10 + index,
      start,
      end,
      score,
      common: null,
      entity: false
    }))
  };
}

function neutralCoreGroup(group: AnalyzerSegmentGroup): SegmentList {
  const info: CalcScoreInfo = {
    posi: ['n'],
    seqSet: [8_000_000 + group.groupId],
    conj: [],
    common: null,
    scoreInfo: [1, null, 0, null],
    kpcl: [true, true, false, true]
  };
  return {
    start: group.start,
    end: group.end,
    matches: group.matches,
    segments: group.segments.map((segment, index) => {
      const primary = {
        id: group.groupId * 10 + index,
        seq: 8_000_000 + group.groupId * 10 + index,
        text: `字${group.groupId}${index}`,
        ord: 0,
        common: null,
        commonTags: '',
        conjugateP: false,
        nokanji: false,
        bestKana: 'じ'
      };
      const word: CompoundText = {
        text: primary.text,
        kana: 'じ',
        primary,
        words: [primary],
        seq: [primary.seq],
        scoreMod: 0
      };
      return {
        start: group.start,
        end: group.end,
        word,
        score: segment.score,
        info
      };
    })
  };
}

function portableSignature(parts: readonly AnalyzerPathPart[]): string[] {
  return parts
    .filter((part): part is AnalyzerSegmentGroup => 'segments' in part)
    .map((group) => `${group.start}:${group.end}:${group.segments[0]?.score ?? 0}`);
}

function currentSignature(parts: readonly unknown[]): string[] {
  return parts
    .filter((part): part is SegmentList => Boolean(
      part && typeof part === 'object' && 'segments' in part
    ))
    .map((group) => `${group.start}:${group.end}:${group.segments[0]?.score ?? 0}`);
}

describe('portable analyzer path DP', () => {
  test('differentially matches the current neutral-transition DP', async () => {
    const groups = [
      portableGroup(0, 0, 1, [70, 60]),
      portableGroup(1, 0, 2, [130]),
      portableGroup(2, 1, 3, [115]),
      portableGroup(3, 2, 4, [125]),
      portableGroup(4, 4, 5, [40])
    ];
    const entities: AnalyzerEntityHint[] = [{ start: 0, end: 2, boost: 25 }];
    const current = await currentFindBestPath(
      groups.map(neutralCoreGroup),
      5,
      { limit: 5, entities }
    );
    const portable = findAnalyzerPaths(groups, 5, { limit: 5, entities });

    expect(portable.map((path) => path.score)).toEqual(current.map((path) => path[1]));
    expect(portable.map((path) => portableSignature(path.parts))).toEqual(
      current.map((path) => currentSignature(path[0]))
    );
  });

  test('keeps insertion order for equal scores and honors the exact limit', () => {
    const groups = [
      portableGroup(10, 0, 1, [100]),
      portableGroup(11, 0, 1, [100]),
      portableGroup(12, 0, 1, [100])
    ];
    const paths = findAnalyzerPaths(groups, 1, { limit: 2 });
    expect(paths).toHaveLength(2);
    expect(paths.map((path) =>
      (path.parts[0] as AnalyzerSegmentGroup).groupId
    )).toEqual([10, 11]);
  });

  test('scores filtered pair transitions with replacement groups and adjustments', () => {
    const left = portableGroup(20, 0, 1, [20, 10]);
    const right = portableGroup(21, 1, 2, [18, 7]);
    const filteredLeft = { ...left, segments: [left.segments[1]!] };
    const filteredRight = { ...right, segments: [right.segments[1]!] };
    const result = findAnalyzerPaths([left, right], 2, {
      limit: 3,
      transition: (_left, _right) => [{
        right: filteredRight,
        adjustment: {
          score: 50,
          start: 1,
          end: 1,
          description: 'fixture',
          connector: ' '
        },
        left: filteredLeft
      }]
    });
    expect(result[0]!.score).toBe(67);
    expect(result[0]!.parts.map((part) => 'segments' in part ? part.groupId : part.score)).toEqual([
      20, 50, 21
    ]);
  });

  test('preserves the current double application of synthetic entity boosts', () => {
    const entity = { start: 0, end: 2, boost: 75 };
    const groups = addAnalyzerEntityGroups([], [entity]);
    expect(groups[0]!.segments[0]!.score).toBe(75);
    const paths = findAnalyzerPaths(groups, 2, { entities: [entity] });
    expect(paths[0]!.score).toBe(150);
    expect((paths[0]!.parts[0] as AnalyzerSegmentGroup).segments[0]!.entity).toBe(true);
  });

  test('rejects an allocation-sized result limit before constructing top-N buffers', () => {
    expect(() => findAnalyzerPaths([], 0, { limit: 100_000_000 })).toThrow(
      'limit must be an integer from 1 to 10'
    );
  });

  test('materializes a long best path only after the dense transition graph is scored', () => {
    const groupCount = 2_000;
    const groups = Array.from(
      { length: groupCount },
      (_, index) => portableGroup(index, index, index + 1, [1])
    );
    let transitions = 0;
    const paths = findAnalyzerPaths(groups, groupCount, {
      limit: 1,
      initial: group => [group],
      transition: (left, right) => {
        transitions++;
        return [{ left, right }];
      }
    });

    expect(transitions).toBe(groupCount * (groupCount - 1) / 2);
    expect(paths[0]?.parts).toHaveLength(groupCount);
  });

  test('matches the complete default-rule graph across dense gaps and stable ties', () => {
    let state = 0x51ee_71e5;
    const random = (): number => {
      state = (Math.imul(state, 1664525) + 1013904223) >>> 0;
      return state;
    };
    for (let iteration = 0; iteration < 80; iteration++) {
      const textLength = 12 + random() % 9;
      const groups: AnalyzerSegmentGroup[] = [];
      let groupId = 1;
      for (let start = 0; start < textLength; start++) {
        for (let length = 1; length <= 4 && start + length <= textLength; length++) {
          if (random() % 4 === 0) continue;
          const group = portableGroup(groupId++, start, start + length, [
            5 + random() % 8,
            5 + random() % 8
          ]);
          const strong = random() % 3 === 0;
          groups.push({
            ...group,
            segments: group.segments.map((segment, index) => ({
              ...segment,
              rules: {
                text: index === 0 && random() % 5 === 0 ? 'と' : 'あ',
                wordKind: 'simple',
                scoreInfo: {
                  positions: [],
                  seqSet: random() % 7 === 0 ? [1342560] : [],
                  conjugations: [], common: null,
                  breakdown: {
                    propertyScore: 1, kanjiBreak: null, useLengthBonus: 0, split: null
                  },
                  flags: strong ? ANALYZER_SCORE_FLAG_STRONG : 0
                },
                compoundEndSeq: null,
                compoundEndText: null
              }
            }))
          });
        }
      }
      const entities: AnalyzerEntityHint[] = [{
        start: random() % (textLength - 1),
        end: textLength,
        boost: (random() % 5) - 2
      }];
      const optimized = findAnalyzerPaths(groups, textLength, { limit: 10, entities });
      const complete = findAnalyzerPaths(groups, textLength, {
        limit: 10,
        entities,
        // Wrapping the resolver deliberately selects the exhaustive custom-rule path.
        transition: (left, right) => resolveAnalyzerRuleTransitions(left, right)
      });
      expect(optimized).toEqual(complete);
    }
  });

  test('keeps exhaustive gap semantics when a custom initial resolver changes spans', () => {
    const groups = [
      portableGroup(1, 0, 1, [10]),
      portableGroup(2, 2, 3, [10])
    ];
    const initial = (group: AnalyzerSegmentGroup): readonly AnalyzerSegmentGroup[] => [
      group.groupId === 1 ? { ...group, end: 2 } : group
    ];
    const actual = findAnalyzerPaths(groups, 3, { limit: 5, initial });
    const exhaustive = findAnalyzerPaths(groups, 3, {
      limit: 5,
      initial,
      transition: (left, right) => resolveAnalyzerRuleTransitions(left, right)
    });
    expect(actual).toEqual(exhaustive);
  });
});
