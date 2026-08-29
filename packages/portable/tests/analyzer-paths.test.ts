import { describe, expect, test } from 'bun:test';
import { findBestPath as currentFindBestPath } from '../../core/src/dict/segmentation.js';
import type { CalcScoreInfo, CompoundText, SegmentList } from '../../core/src/types.js';
import {
  addAnalyzerEntityGroups,
  findAnalyzerPaths
} from '../src/analyzer-paths.js';
import type {
  AnalyzerEntityHint,
  AnalyzerPathPart,
  AnalyzerSegmentGroup
} from '../src/analyzer-types.js';

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
});
