import { createHash } from 'node:crypto';
import { describe, expect, test } from 'bun:test';
import {
  canonicalMorphologyDiffLine,
  morphologyRelationAttestation,
  verifyMorphologyRelation,
  type MorphologyRelationDiff
} from '../src/browser-pack/morphology-verifier.js';

const databaseArtifacts = {
  csrRows: 10,
  installedRouteCsrRows: 9,
  activeRouteCsrRows: 8,
  inactiveRouteCsrRows: 1,
  uninstalledCsrRows: 1,
  dualRouteCsrRows: 0,
  ghostSourceRows: 2,
  ghostRootSurfacePairs: 1,
  multiPropertyLinks: 3,
  staleRawKanaSurfaces: 1
};

function relationRow(rootSeq: number, sourceText: string) {
  return {
    route: 'kana',
    surface: 'あ',
    rootSeq,
    sourceText,
    sourceForm: sourceText,
    sourceReading: sourceText,
    intermediate: null,
    firstPos: 'v1',
    firstType: 2,
    firstNegative: false,
    firstFormal: false,
    secondPos: null,
    secondType: null,
    secondNegative: null,
    secondFormal: null,
    ord: 0,
    common: null
  } as const;
}

function fakeSql(rows: readonly ReturnType<typeof relationRow>[]) {
  const sql = () => Promise.resolve([databaseArtifacts]);
  return Object.assign(sql, {
    unsafe: () => ({
      cursor: async (_batchSize: number, consume: (values: typeof rows) => void) => {
        consume(rows);
      }
    })
  });
}

describe('exhaustive morphology verifier', () => {
  test('hashes canonical sorted JSONL while streaming relation groups', async () => {
    const lines: string[] = [];
    const result = await verifyMorphologyRelation({
      lookup: { lookup: () => [] },
      sql: fakeSql([relationRow(2, 'b'), relationRow(1, 'a')]) as never,
      onDiff: (_diff, line) => lines.push(line)
    });
    const expectedDiffs: MorphologyRelationDiff[] = [
      {
        route: 'kana',
        surface: 'あ',
        side: 'legacy-only',
        key: JSON.stringify([1, 'a', 'a', 'a', null, [['v1', 2, false, false]], 0, null])
      },
      {
        route: 'kana',
        surface: 'あ',
        side: 'legacy-only',
        key: JSON.stringify([2, 'b', 'b', 'b', null, [['v1', 2, false, false]], 0, null])
      }
    ];
    const expectedLines = expectedDiffs.map(canonicalMorphologyDiffLine);
    const expectedDigest = createHash('sha256').update(expectedLines.join('')).digest('hex');

    expect(lines).toEqual(expectedLines);
    expect(result.diffRows).toBe(2);
    expect(result.diffSha256).toBe(expectedDigest);
    expect(result.relationRows).toBe(2);
    expect(result.legacyRelationKeys).toBe(2);
    expect(result.alphaRelationKeys).toBe(0);
    expect(result.legacyOnly).toBe(2);
    expect(result.alphaOnly).toBe(0);
    expect(result.duplicateLegacyRows).toBe(0);
    expect(result.duplicateAlphaCandidates).toBe(0);
    expect(result.databaseArtifacts).toEqual(databaseArtifacts);
    expect(morphologyRelationAttestation(result)).toEqual({
      rows: 2,
      sha256: expectedDigest,
      relationRows: 2,
      surfaceGroups: 1,
      exactSurfaceGroups: 0,
      legacyRelationKeys: 2,
      alphaRelationKeys: 0,
      legacyOnly: 2,
      alphaOnly: 0,
      duplicateLegacyRows: 0,
      duplicateAlphaCandidates: 0,
      databaseArtifacts
    });
  });
});
