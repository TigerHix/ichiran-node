import { beforeAll, describe, expect, test } from 'bun:test';
import { fileURLToPath } from 'node:url';
import { openMorphology } from '../../core/src/morphology.js';
import { buildMorphology } from '../src/browser-pack/morphology-compiler.js';
import {
  chronologicalMorphologySource,
  foldChronologicalConjugationErrata,
  manualPatchDigest,
  regeneratedLineageDigest,
  type ChronologicalConjugationFold
} from '../src/source-compiler/conjugation-errata.js';
import {
  applyQualifiedErrata,
  loadQualifiedErrata,
  type AppliedErrata
} from '../src/source-compiler/chronological-errata.js';
import type {
  CanonicalEntry,
  CanonicalForm,
  CanonicalRoute
} from '../src/source-compiler/model.js';

const RULE_DATA = fileURLToPath(new URL('../../../data', import.meta.url));
const ERRATA = fileURLToPath(new URL('../../../data/source-compiler-errata.json', import.meta.url));

interface FormSpec {
  readonly text: string;
  readonly best?: string | null;
  readonly common?: number | null;
}

function forms(values: readonly FormSpec[]): CanonicalForm[] {
  return values.map((value, ordinal) => ({
    text: value.text,
    ordinal,
    sourceOrder: { event: 0, ordinal },
    common: value.common ?? null,
    priorityTags: [],
    conjugatable: true,
    noKanji: false,
    best: value.best ?? null
  }));
}

function entry(
  seq: number,
  position: string,
  routes: Partial<Record<CanonicalRoute, readonly FormSpec[]>>
): CanonicalEntry {
  return {
    seq,
    source: { sourceId: 'qualified-errata-witness', ordinal: seq },
    kanji: forms(routes.kanji ?? []),
    kana: forms(routes.kana ?? []),
    senses: [{
      ordinal: 0,
      glosses: [],
      properties: [{
        tag: 'pos',
        text: position,
        ordinal: 0,
        sourceOrder: { event: 0, ordinal: 0 }
      }]
    }],
    restrictions: [],
    primaryNoKanji: (routes.kanji?.length ?? 0) === 0
  };
}

function baseEntries(): CanonicalEntry[] {
  return [
    entry(1_008_340, 'cop', { kana: [{ text: 'である' }] }),
    entry(2_089_020, 'cop', { kana: [{ text: 'だ', common: 0 }] }),
    entry(1_612_690, 'exp', {
      kanji: [
        { text: '御座います', best: 'ございます' },
        { text: '御座居ます', best: 'ございます' },
        { text: 'ご座います', best: 'ございます' },
        { text: '厶います', best: 'ございます' }
      ],
      kana: [{ text: 'ございます', best: '御座います', common: 0 }]
    }),
    entry(2_253_080, 'exp', {
      kanji: [{ text: 'で御座います', best: 'でございます', common: 0 }],
      kana: [{ text: 'でございます', best: 'で御座います', common: 0 }]
    }),
    // Final source POS no longer emits the historical adj-i type-51 row. The
    // ledger deletion remains as one explicitly named ghost-lineage tombstone.
    entry(2_257_550, 'aux-adj', { kana: [{ text: 'ない' }] }),
    entry(2_684_620, 'adj-i', { kana: [{ text: 'しい' }] }),
    entry(1_008_370, 'adj-i', { kana: [{ text: 'でかい' }] }),
    entry(1_572_760, 'adj-i', { kana: [{ text: 'くどい' }] }),
    entry(1_593_170, 'v1', { kana: [{ text: 'こける' }] }),
    entry(1_566_420, 'v1', { kana: [{ text: 'はめる' }] }),
    entry(1_584_060, 'v5m', { kana: [{ text: 'くるむ' }, { text: 'つつむ' }] }),
    entry(1_602_880, 'v5s', { kanji: [{ text: '殖やす' }, { text: '増やす' }], kana: [{ text: 'ふやす' }] }),
    entry(1_980_880, 'v5s', { kanji: [{ text: '掛け直す' }, { text: 'かけ直す' }], kana: [{ text: 'かけなおす' }] }),
    entry(2_863_544, 'adj-i', { kana: [{ text: 'みぎにでるのはいない' }] })
  ];
}

let applied: AppliedErrata;
let fold: ChronologicalConjugationFold;

beforeAll(async () => {
  const ledger = await loadQualifiedErrata(ERRATA);
  expect(ledger.rows).toHaveLength(601);
  applied = applyQualifiedErrata(baseEntries(), ledger, 0);
  fold = foldChronologicalConjugationErrata(applied.entries, applied.conjugationRows, {
    dataPath: RULE_DATA
  });
});

describe('chronological conjugation errata', () => {
  test('folds every conjugation-affecting declaration from the 601-row ledger', () => {
    expect(applied.conjugationRows.map(row => [row.event, row.operation])).toEqual([
      [0, 'conjugateDa'],
      [1, 'addDehaJaReadings'],
      [3, 'addGozaimasuConjs'],
      [188, 'rearrangeReadingsConj'],
      [190, 'rearrangeReadingsConj'],
      [193, 'deleteConjugation'],
      [194, 'deleteConjugation'],
      [347, 'addConjReading'],
      [349, 'addConjReading'],
      [373, 'addConjReading'],
      [435, 'rearrangeReadingsConj'],
      [456, 'addConjReading'],
      [493, 'replaceReadingConj']
    ]);
    expect(fold.counts).toEqual({
      rows: 13,
      dehaJaPatches: 8,
      gozaimasuPatches: 42,
      manualPatches: 50,
      suppressions: 2,
      regeneratedReadings: 4,
      regeneratedLineages: 162,
      reorderedReadings: 3,
      replacedReadings: 1
    });
  });

  test('enumerates addConjReading as source-native primary lineages', () => {
    expect(fold.regeneratedLineages).toHaveLength(162);
    expect(regeneratedLineageDigest(fold.regeneratedLineages))
      .toBe('04d77a77d0113eea9f878f2468c0833e9c26fe1bf42865adbd952f25795dd408');
    expect(Object.fromEntries([1_008_370, 1_566_420, 1_572_760, 1_593_170].map(seq => [
      seq,
      fold.regeneratedLineages.filter(row => row.rootSeq === seq).length
    ]))).toEqual({
      1008370: 22,
      1566420: 59,
      1572760: 22,
      1593170: 59
    });
  });

  test('reproduces the complete qualified manual-patch projection', () => {
    expect(manualPatchDigest(fold.manualPatches))
      .toBe('ec2957e97afd0421a567febdd181ec1743450874a1cc3fd0aa8d3148d4e7e022');
    expect(fold.manualPatches.filter(row => row.rootSeq === 2_089_020).map(row => row.surface)).toEqual([
      'じゃありません',
      'じゃありませんでした',
      'じゃありませんでしたら',
      'じゃない',
      'じゃないです',
      'じゃなかった',
      'じゃなかったら',
      'じゃなくて'
    ]);
    expect(fold.manualPatches.filter(row => row.rootSeq === 1_612_690)).toHaveLength(30);
    expect(fold.manualPatches.filter(row => row.rootSeq === 2_253_080)).toHaveLength(12);
  });

  test('translates physical delete declarations into two reviewed semantic suppressions', () => {
    expect(fold.suppressions).toEqual([
      {
        route: 'kana',
        rootSeq: 2_257_550,
        sourceText: 'ない',
        surface: 'な',
        first: { pos: 'adj-i', type: 51, negative: null, formal: null },
        second: null,
        provenance: { event: 193, sourceLine: 1474, oracleTargetSeq: 2_029_110 }
      },
      {
        route: 'kana',
        rootSeq: 2_684_620,
        sourceText: 'しい',
        surface: 'し',
        first: { pos: 'adj-i', type: 51, negative: null, formal: null },
        second: null,
        provenance: { event: 194, sourceLine: 1475, oracleTargetSeq: 2_086_640 }
      }
    ]);
  });
});

describe('chronological MorphologySource adapter', () => {
  test('builds qualified format-v1 morphology with PostgreSQL unavailable', () => {
    const priorDatabase = process.env.DATABASE_URL;
    const priorIchiran = process.env.ICHIRAN_DB_URL;
    process.env.DATABASE_URL = 'postgres://postgres-unavailable.invalid/ichiran';
    process.env.ICHIRAN_DB_URL = 'postgres://postgres-unavailable.invalid/ichiran';
    try {
      const source = chronologicalMorphologySource(applied.entries, applied.conjugationRows, {
        dataPath: RULE_DATA,
        extraPositions: [{ seq: 1_008_340, pos: 'cop' }]
      });
      expect(source.manualPatches).toHaveLength(50);
      expect(source.roots.some(root => root.seq === 1_008_340 && root.pos === 'cop')).toBe(true);
      const build = buildMorphology(source, { dataPath: RULE_DATA });
      expect(build.stats.patches).toBe(50);
      expect(build.stats.tombstones).toBe(4);

      const morphology = openMorphology(build.bytes);
      expect(morphology.lookup('じゃない', 'kana').some(value => value.rootSeq === 2_089_020)).toBe(true);
      expect(morphology.lookup('ございません', 'kana').some(value => value.rootSeq === 1_612_690)).toBe(true);
      expect(morphology.lookup('な', 'kana').some(value => value.rootSeq === 2_257_550)).toBe(false);
      expect(morphology.lookup('し', 'kana').some(value => value.rootSeq === 2_684_620)).toBe(false);
    } finally {
      if (priorDatabase === undefined) delete process.env.DATABASE_URL;
      else process.env.DATABASE_URL = priorDatabase;
      if (priorIchiran === undefined) delete process.env.ICHIRAN_DB_URL;
      else process.env.ICHIRAN_DB_URL = priorIchiran;
    }
  });
});
