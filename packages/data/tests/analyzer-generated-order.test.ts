import { describe, expect, test } from 'bun:test';
import { buildAnalyzerAnnotations } from '../src/browser-pack/analyzer-annotations.js';
import {
  compileLookupOrders,
  type LookupOrderRow
} from '../src/browser-pack/analyzer-generated.js';
import type { AnalyzerSupportGeneratedSource } from '../src/browser-pack/analyzer-support.js';

const TOTALS = {
  physicalClasses: 8,
  locatedClasses: 8,
  ambiguousSurfaces: 4,
  loadedPatches: 0
} as const;

function row(
  route: LookupOrderRow['route'],
  surface: string,
  rank: number,
  rootSeq: number,
  firstAlias: number | null,
  secondAlias: number | null = null
): LookupOrderRow {
  return {
    route, surface, rank, rootSeq, firstAlias, secondAlias,
    ...TOTALS
  };
}

const ROWS: readonly LookupOrderRow[] = [
  // A has two semantic locators for one physical class. A < B here and B < A
  // below intentionally form an SCC, forcing both surfaces into exact local
  // exceptions while retaining one deterministic global component.
  row('kanji', '甲', 0, 10, null),
  row('kanji', '甲', 0, 11, 0),
  row('kanji', '甲', 1, 20, 1),
  row('kanji', '乙', 0, 20, 1),
  row('kanji', '乙', 1, 10, null),
  row('kanji', '乙', 1, 11, 0),
  // The remaining surfaces form an acyclic A/B -> C -> D chain and therefore
  // exercise the global longest-path projection as well.
  row('kanji', '丁', 0, 10, null),
  row('kanji', '丁', 0, 11, 0),
  row('kanji', '丁', 1, 30, null),
  row('kanji', '丙', 0, 30, null),
  row('kanji', '丙', 1, 40, 2)
];

function shuffled(values: readonly LookupOrderRow[], seed: number): LookupOrderRow[] {
  const result = [...values];
  let state = seed >>> 0;
  for (let index = result.length - 1; index > 0; index--) {
    state = (Math.imul(state, 1_664_525) + 1_013_904_223) >>> 0;
    const swap = state % (index + 1);
    [result[index], result[swap]] = [result[swap]!, result[index]!];
  }
  return result;
}

function annotationsBytes(rows: readonly LookupOrderRow[]): Uint8Array {
  const order = compileLookupOrders(rows, 3, 0);
  const generated: AnalyzerSupportGeneratedSource = {
    ruleAliases: [], aliasCount: 3, records: [], semanticPaths: 0,
    matchedPaths: 0, countExceptions: 0,
    lookupOrders: order.values,
    lookupOrderSourceRows: order.sourceRows,
    lookupOrderSourceSha256: order.sourceSha256,
    lookupOrderSurfaces: order.surfaces,
    lookupOrderClasses: order.physicalClasses,
    lookupOrderEquivalenceClasses: order.equivalenceClasses,
    lookupOrderComponents: order.components,
    lookupOrderCyclicComponents: order.cyclicComponents,
    lookupOrderEdges: order.edges,
    lookupOrderMaxRank: order.maxRank,
    lookupOrderProjectionSha256: order.sha256,
    lookupOrderExceptions: order.exceptions,
    lookupOrderExceptionClasses: order.exceptionClasses,
    lookupOrderExceptionLocators: order.exceptionLocators,
    physicalGroups: 0, physicalMembers: 0, propertyOverrides: 0,
    maxMemberOrd: 0, maxViaMemberOrd: 0, maxPropOrd: 0,
    projectionSha256: 'fixture'
  };
  return buildAnalyzerAnnotations([], [], generated).bytes;
}

describe('lookup-order projection determinism', () => {
  test('is invariant to SQL/input row order, including encoded bytes', () => {
    const baseline = compileLookupOrders(ROWS, 3, 0);
    const baselineBytes = annotationsBytes(ROWS);

    expect(baseline).toMatchObject({
      sourceRows: 11,
      surfaces: 4,
      physicalClasses: 8,
      equivalenceClasses: 4,
      components: 3,
      cyclicComponents: 1,
      edges: 4,
      maxRank: 2,
      exceptionClasses: 4,
      exceptionLocators: 6
    });

    const permutations: LookupOrderRow[][] = [
      [...ROWS].reverse(),
      ...ROWS.map((_, offset) => [...ROWS.slice(offset), ...ROWS.slice(0, offset)]),
      ...Array.from({ length: 128 }, (_, seed) => shuffled(ROWS, seed + 1))
    ];
    for (const values of permutations) {
      expect(compileLookupOrders(values, 3, 0)).toEqual(baseline);
      expect(annotationsBytes(values)).toEqual(baselineBytes);
    }
  });
});
