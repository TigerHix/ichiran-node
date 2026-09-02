import { describe, expect, test } from 'bun:test';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { buildAnalyzerAnnotations } from '../src/browser-pack/analyzer-annotations.js';
import { buildMorphology } from '../src/browser-pack/morphology-compiler.js';
import type { CompiledMorphologyArtifact } from '../src/browser-pack/morphology-format.js';
import { compileBoundedGeneratedProjection } from '../src/source-compiler/analyzer-generated-stream.js';
import type { PhysicalTarget } from '../src/source-compiler/conjugation-emissions-physical.js';
import { conjugationPositionsByRoot } from '../src/source-compiler/conjugation-emission-order.js';
import {
  writeScheduledGeneratedProjection,
  type GeneratedProjectionStreamResult
} from '../src/source-compiler/generated-projection-stream.js';
import { GeneratedProjectionSpoolWriter } from '../src/source-compiler/generated-projection-spool.js';
import {
  CONJUGATION_PHASE,
  conjugationPhasePrecedence,
  iterateScheduledConjugations
} from '../src/source-compiler/conjugation-scheduler.js';
import {
  reduceGeneratedOccurrenceSurfaces,
  reduceGeneratedSemanticPaths
} from '../src/source-compiler/generated-projection-reduce.js';
import type { CanonicalEntry } from '../src/source-compiler/model.js';

const dataPath = fileURLToPath(new URL('../../../data', import.meta.url));

function entry(): CanonicalEntry {
  return {
    seq: 1_519_210,
    source: { sourceId: 'fixture', ordinal: 0 },
    kanji: [{
      text: '忘れる', ordinal: 0, sourceOrder: { event: 0, ordinal: 0 },
      common: null, priorityTags: [], conjugatable: true, best: 'わすれる'
    }],
    kana: [{
      text: 'わすれる', ordinal: 0, sourceOrder: { event: 0, ordinal: 0 },
      common: null, priorityTags: [], conjugatable: true, noKanji: false, best: '忘れる'
    }],
    senses: [{ ordinal: 0, properties: [], glosses: ['forget'] }],
    restrictions: [],
    primaryNoKanji: false
  };
}

function scheduledEntry(
  seq: number,
  kanji: string,
  kana: string,
  sourceOrdinal: number
): CanonicalEntry {
  return {
    seq,
    source: { sourceId: 'scheduler-fixture', ordinal: sourceOrdinal },
    kanji: [{
      text: kanji, ordinal: 0, sourceOrder: { event: 0, ordinal: 0 },
      common: null, priorityTags: [], conjugatable: true, noKanji: false, best: kana
    }],
    kana: [{
      text: kana, ordinal: 0, sourceOrder: { event: 0, ordinal: 0 },
      common: null, priorityTags: [], conjugatable: true, noKanji: false, best: kanji
    }],
    senses: [],
    restrictions: [],
    primaryNoKanji: false
  };
}

function physicalTarget(
  seq: number,
  kanji: string,
  kana: string,
  origin: PhysicalTarget['origin']
): PhysicalTarget {
  return {
    seq,
    kanji: [kanji],
    kana: [kana],
    secondaryForms: [],
    conjugatable: origin === 'lexical',
    origin
  };
}

function generatedMemberFixture(directory: string): {
  readonly entries: readonly CanonicalEntry[];
  readonly morphology: CompiledMorphologyArtifact;
  readonly projection: GeneratedProjectionStreamResult;
} {
  const entries = [
    scheduledEntry(10, '甲', 'こう', 0),
    scheduledEntry(20, '乙', 'おつ', 1),
    scheduledEntry(30, '丙', 'へい', 2)
  ];
  const pathsPath = join(directory, 'paths.bin');
  const occurrencesPath = join(directory, 'occurrences.bin');
  const writer = new GeneratedProjectionSpoolWriter(pathsPath, occurrencesPath);
  writer.writePath({
    ordinal: 0, rootSeq: 10, firstAlias: 0, secondAlias: null,
    targetSeq: 100, viaTargetSeq: null
  });
  writer.writePath({
    ordinal: 1, rootSeq: 20, firstAlias: 0, secondAlias: null,
    targetSeq: 100, viaTargetSeq: null
  });
  writer.writePath({
    ordinal: 2, rootSeq: 20, firstAlias: 0, secondAlias: 1,
    targetSeq: 101, viaTargetSeq: 100
  });
  writer.writePath({
    ordinal: 3, rootSeq: 30, firstAlias: 0, secondAlias: null,
    targetSeq: 102, viaTargetSeq: null
  });
  const spool = writer.close();
  return {
    entries,
    morphology: {
      positions: ['v1'],
      rules: [],
      templates: [],
      rootKeys: [],
      rootGroups: [],
      patches: [],
      tombstones: []
    },
    projection: {
      pathsPath,
      occurrencesPath,
      spool,
      targets: [
        physicalTarget(10, '甲', 'こう', 'lexical'),
        physicalTarget(20, '乙', 'おつ', 'lexical'),
        physicalTarget(30, '丙', 'へい', 'lexical'),
        physicalTarget(100, '共有', 'きょうゆう', 'generated'),
        physicalTarget(101, '連鎖', 'れんさ', 'generated'),
        physicalTarget(102, '単独', 'たんどく', 'generated')
      ],
      ruleAliases: [0, 1],
      aliasProperties: [
        { pos: 'v1', type: 1, negative: false, formal: false },
        { pos: 'v1', type: 2, negative: false, formal: false }
      ],
      phases: {},
      patches: 0,
      regeneratedTargetForms: 0
    }
  };
}

describe('bounded generated projection producer', () => {
  test('synthesizes singleton members while retaining shared and via ordinals', async () => {
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-generated-members-'));
    try {
      const fixture = generatedMemberFixture(directory);
      const compiled = compileBoundedGeneratedProjection({
        ...fixture,
        temporaryDirectory: directory,
        customRootSeqs: new Set(),
        firstErrataEvent: 10,
        maxOccurrenceChunkRows: 100
      });
      expect(compiled.generated.semanticPaths).toBe(4);
      expect(compiled.generated.matchedPaths).toBe(4);
      expect(compiled.generated.countExceptions).toBe(0);
      expect(compiled.generated.physicalGroups).toBe(1);
      expect(compiled.generated.physicalMembers).toBe(3);
      expect(compiled.generated.maxMemberOrd).toBe(1);
      expect(compiled.generated.maxViaMemberOrd).toBe(1);
      expect(compiled.generated.records).toEqual([
        {
          rootSeq: 10,
          firstAlias: 0,
          secondAlias: null,
          counts: null,
          physicalGroup: 1,
          members: [{
            property: { posId: 0, type: 1, negative: false, formal: false },
            memberOrd: 0,
            propOrd: 0,
            viaMemberOrd: null
          }]
        },
        {
          rootSeq: 20,
          firstAlias: 0,
          secondAlias: null,
          counts: null,
          physicalGroup: 1,
          members: [{
            property: { posId: 0, type: 1, negative: false, formal: false },
            memberOrd: 1,
            propOrd: 0,
            viaMemberOrd: null
          }]
        },
        {
          rootSeq: 20,
          firstAlias: 0,
          secondAlias: 1,
          counts: null,
          physicalGroup: null,
          members: [{
            property: { posId: 0, type: 2, negative: false, formal: false },
            memberOrd: 0,
            propOrd: 0,
            viaMemberOrd: 1
          }]
        }
      ]);
      expect(compiled.generated.records.some(record => record.rootSeq === 30)).toBe(false);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  test('rejects a non-contiguous generated physical target tail', async () => {
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-generated-target-tail-'));
    try {
      const fixture = generatedMemberFixture(directory);
      const targets = fixture.projection.targets.map(target =>
        target.seq === 101 ? { ...target, seq: 103 } : target);
      expect(() => compileBoundedGeneratedProjection({
        ...fixture,
        projection: { ...fixture.projection, targets },
        temporaryDirectory: directory,
        customRootSeqs: new Set(),
        firstErrataEvent: 10,
        maxOccurrenceChunkRows: 100
      })).toThrow('Generated physical target tail is not contiguous: expected 101, got 103');
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  test('streams scheduled paths and occurrences into the concrete spools', async () => {
    const root = entry();
    const source = {
      roots: [
        { seq: root.seq, pos: 'v1', route: 'kana' as const, text: 'わすれる', ord: 0, common: null, counterpart: '忘れる' },
        { seq: root.seq, pos: 'v1', route: 'kanji' as const, text: '忘れる', ord: 0, common: null, counterpart: 'わすれる' },
        { seq: 2_257_550, pos: 'adj-i', route: 'kana' as const, text: 'ない', ord: 0, common: null, counterpart: null },
        { seq: 2_684_620, pos: 'adj-i', route: 'kana' as const, text: 'しい', ord: 0, common: null, counterpart: null }
      ],
      rootForms: [
        { seq: root.seq, text: '忘れる' },
        { seq: root.seq, text: 'わすれる' },
        { seq: 2_257_550, text: 'ない' },
        { seq: 2_684_620, text: 'しい' }
      ],
      manualPatches: []
    };
    const morphology = buildMorphology(source, { dataPath }).artifact;
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-generated-stream-'));
    const pathsPath = join(directory, 'paths.bin');
    const occurrencesPath = join(directory, 'occurrences.bin');
    try {
      const result = writeScheduledGeneratedProjection({
        entries: [root],
        positionsByRoot: conjugationPositionsByRoot(source),
        customRootSeqs: new Set(),
        firstErrataEvent: 10,
        chronologicalPositions: [],
        suppressions: [],
        regeneratedLineages: [],
        physicalTargetOrderCompatibility: [],
        lineageCompatibility: [],
        morphology,
        firstGeneratedSeq: 10_000_000,
        pathsPath,
        occurrencesPath
      });
      expect(result.spool.paths).toBeGreaterThan(0);
      expect(result.spool.occurrences).toBeGreaterThan(result.spool.paths);
      expect(result.patches).toBe(0);
      const semantic = reduceGeneratedSemanticPaths(pathsPath, () => {});
      expect(semantic.paths).toBe(result.spool.paths);
      const surfaces = reduceGeneratedOccurrenceSurfaces({
        pathsPath,
        occurrencesPath,
        temporaryDirectory: directory,
        maxChunkRows: 100
      }, () => {});
      expect(surfaces.rows).toBe(result.spool.installedOccurrences);
      expect(surfaces.surfaces).toBeGreaterThan(0);
      const compiled = compileBoundedGeneratedProjection({
        projection: result,
        entries: [root],
        morphology,
        temporaryDirectory: directory,
        customRootSeqs: new Set(),
        firstErrataEvent: 10,
        maxOccurrenceChunkRows: 100
      });
      expect(compiled.generated.matchedPaths).toBe(compiled.generated.semanticPaths);
      const encoded = buildAnalyzerAnnotations([], [], compiled.generated);
      expect(encoded.bytes.byteLength).toBeGreaterThan(0);
      expect(encoded.stats.generatedRecords).toBe(compiled.generated.records.reduce(
        (sum, record) => sum + (record.members?.length ?? 1),
        0
      ));
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  test('the replayable scheduler owns dense global phase order', () => {
    const base = scheduledEntry(700_001, '決める', 'きめる', 0);
    const custom = scheduledEntry(700_002, '止める', 'とめる', 1);
    const daSeed = scheduledEntry(2_089_020, 'だ', 'だ', 2);
    const da: CanonicalEntry = {
      ...daSeed,
      kanji: [],
      kana: [{ ...daSeed.kana[0]!, best: null }],
      primaryNoKanji: true
    };
    const roots = [
      { entry: base, pos: 'v1' },
      { entry: custom, pos: 'v1' },
      { entry: da, pos: 'cop' }
    ];
    const source = {
      roots: [
        ...roots.flatMap(({ entry: value, pos }) => [
          ...value.kanji.map(form => ({
            seq: value.seq, pos, route: 'kanji' as const, text: form.text,
            ord: form.ordinal, common: null, counterpart: form.best
          })),
          ...value.kana.map(form => ({
            seq: value.seq, pos, route: 'kana' as const, text: form.text,
            ord: form.ordinal, common: null, counterpart: form.best
          }))
        ]),
        { seq: 2_257_550, pos: 'adj-i', route: 'kana' as const,
          text: 'ない', ord: 0, common: null, counterpart: null },
        { seq: 2_684_620, pos: 'adj-i', route: 'kana' as const,
          text: 'しい', ord: 0, common: null, counterpart: null }
      ],
      rootForms: [
        ...roots.flatMap(({ entry: value }) =>
          [...value.kanji, ...value.kana].map(form => ({ seq: value.seq, text: form.text }))),
        { seq: 2_257_550, text: 'ない' },
        { seq: 2_684_620, text: 'しい' }
      ],
      manualPatches: []
    };
    const morphology = buildMorphology(source, { dataPath }).artifact;
    const rows = [...iterateScheduledConjugations({
      entries: [custom, da, base],
      positionsByRoot: conjugationPositionsByRoot(source),
      customRootSeqs: new Set([custom.seq]),
      firstErrataEvent: 50,
      chronologicalPositions: [{ rootSeq: da.seq, pos: 'cop', event: 50 }],
      suppressions: [],
      lineageCompatibility: [],
      morphology
    })];
    const phases = new Map<number, Set<number>>();
    for (const row of rows) {
      const values = phases.get(row.emission.rootSeq) ?? new Set<number>();
      values.add(row.phase);
      phases.set(row.emission.rootSeq, values);
    }
    expect(phases.get(base.seq)).toEqual(new Set([
      CONJUGATION_PHASE.basePrimary,
      CONJUGATION_PHASE.baseSecondary
    ]));
    expect(phases.get(custom.seq)).toEqual(new Set([
      CONJUGATION_PHASE.customPrimary,
      CONJUGATION_PHASE.customSecondary
    ]));
    expect(phases.get(da.seq)).toEqual(new Set([CONJUGATION_PHASE.chronological]));
    expect(rows.map(row => row.ordinal)).toEqual(rows.map((_, ordinal) => ordinal));
    expect(conjugationPhasePrecedence(CONJUGATION_PHASE.customPrimary, 7))
      .toBeGreaterThan(conjugationPhasePrecedence(CONJUGATION_PHASE.baseSecondary, 99_999_999));
    expect(() => conjugationPhasePrecedence(CONJUGATION_PHASE.basePrimary, 100_000_000))
      .toThrow('outside its phase');
  });

  test('rejects stale conjugation-reading lineage compatibility', () => {
    const root = scheduledEntry(700_001, '決める', 'きめる', 0);
    const source = {
      roots: [
        { seq: root.seq, pos: 'v1', route: 'kanji' as const, text: '決める', ord: 0, common: null, counterpart: 'きめる' },
        { seq: root.seq, pos: 'v1', route: 'kana' as const, text: 'きめる', ord: 0, common: null, counterpart: '決める' },
        { seq: 2_257_550, pos: 'adj-i', route: 'kana' as const, text: 'ない', ord: 0, common: null, counterpart: null },
        { seq: 2_684_620, pos: 'adj-i', route: 'kana' as const, text: 'しい', ord: 0, common: null, counterpart: null }
      ],
      rootForms: [
        { seq: root.seq, text: '決める' }, { seq: root.seq, text: 'きめる' },
        { seq: 2_257_550, text: 'ない' }, { seq: 2_684_620, text: 'しい' }
      ],
      manualPatches: []
    };
    const morphology = buildMorphology(source, { dataPath }).artifact;
    expect(() => [...iterateScheduledConjugations({
      entries: [root],
      positionsByRoot: conjugationPositionsByRoot(source),
      customRootSeqs: new Set(),
      firstErrataEvent: 10,
      chronologicalPositions: [],
      suppressions: [],
      lineageCompatibility: [{
        id: 'stale-lineage-witness',
        kind: 'conjugation-reading-lineage',
        seq: root.seq,
        route: 'kana',
        sourceText: 'きめる',
        rule: {
          pos: 'v1', type: 999, negative: false, formal: false,
          order: 0, stem: 1, okuri: '', euphr: '', euphk: ''
        },
        lineageStep: 'first',
        provenance: { source: 'test' },
        preservedBehavior: 'This deliberately stale row must be rejected.'
      }],
      morphology
    })]).toThrow('stale-lineage-witness is stale');
  });

  test('allocates a semantic path for a manual-only morphology root', async () => {
    const root = entry();
    const source = {
      roots: [
        { seq: 1_593_170, pos: 'v1', route: 'kana' as const, text: 'コケる', ord: 0, common: null, counterpart: null },
        { seq: 2_257_550, pos: 'adj-i', route: 'kana' as const, text: 'ない', ord: 0, common: null, counterpart: null },
        { seq: 2_684_620, pos: 'adj-i', route: 'kana' as const, text: 'しい', ord: 0, common: null, counterpart: null }
      ],
      rootForms: [
        { seq: root.seq, text: '忘れる' }, { seq: root.seq, text: 'わすれる' },
        { seq: 1_593_170, text: 'コケる' },
        { seq: 2_257_550, text: 'ない' }, { seq: 2_684_620, text: 'しい' }
      ],
      manualPatches: [{
        route: 'kana' as const,
        surface: 'わすれた',
        rootSeq: root.seq,
        pos: 'v1',
        conjType: 2,
        negative: false,
        formal: false,
        sourceText: 'わすれる',
        sourceCounterpart: '忘れる',
        targetCounterpart: '忘れた',
        ord: 0,
        common: null
      }]
    };
    const morphology = buildMorphology(source, { dataPath }).artifact;
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-generated-patch-stream-'));
    try {
      const result = writeScheduledGeneratedProjection({
        entries: [root],
        positionsByRoot: conjugationPositionsByRoot(source),
        customRootSeqs: new Set(),
        firstErrataEvent: 10,
        chronologicalPositions: [],
        suppressions: [],
        regeneratedLineages: [],
        physicalTargetOrderCompatibility: [],
        lineageCompatibility: [],
        morphology,
        firstGeneratedSeq: 10_000_000,
        pathsPath: join(directory, 'paths.bin'),
        occurrencesPath: join(directory, 'occurrences.bin')
      });
      expect(result.spool).toEqual({ paths: 1, occurrences: 1, installedOccurrences: 1 });
      expect(result.patches).toBe(1);
      expect(result.phases).toEqual({ 6: 1 });
      expect(result.targets.at(-1)).toEqual(expect.objectContaining({
        kanji: ['忘れた'],
        kana: ['わすれた'],
        origin: 'generated'
      }));
      const compiled = compileBoundedGeneratedProjection({
        projection: result,
        entries: [root],
        morphology,
        temporaryDirectory: directory,
        customRootSeqs: new Set(),
        firstErrataEvent: 10,
        maxOccurrenceChunkRows: 100
      });
      expect(compiled.generated.matchedPaths).toBe(compiled.generated.semanticPaths);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  test('counts one extra match when a patch shares a rule-derived path', async () => {
    const root = entry();
    const source = {
      roots: [
        { seq: root.seq, pos: 'v1', route: 'kana' as const, text: 'わすれる', ord: 0, common: null, counterpart: '忘れる' },
        { seq: root.seq, pos: 'v1', route: 'kanji' as const, text: '忘れる', ord: 0, common: null, counterpart: 'わすれる' },
        { seq: 2_257_550, pos: 'adj-i', route: 'kana' as const, text: 'ない', ord: 0, common: null, counterpart: null },
        { seq: 2_684_620, pos: 'adj-i', route: 'kana' as const, text: 'しい', ord: 0, common: null, counterpart: null }
      ],
      rootForms: [
        { seq: root.seq, text: '忘れる' }, { seq: root.seq, text: 'わすれる' },
        { seq: 2_257_550, text: 'ない' }, { seq: 2_684_620, text: 'しい' }
      ],
      manualPatches: [{
        route: 'kana' as const,
        surface: 'わすれた',
        rootSeq: root.seq,
        pos: 'v1',
        conjType: 2,
        negative: false,
        formal: false,
        sourceText: 'わすれる',
        sourceCounterpart: '忘れる',
        targetCounterpart: '忘れた',
        ord: 0,
        common: null
      }]
    };
    const morphology = buildMorphology(source, { dataPath }).artifact;
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-generated-overlap-stream-'));
    try {
      const result = writeScheduledGeneratedProjection({
        entries: [root],
        positionsByRoot: conjugationPositionsByRoot(source),
        customRootSeqs: new Set(),
        firstErrataEvent: 10,
        chronologicalPositions: [],
        suppressions: [],
        regeneratedLineages: [],
        physicalTargetOrderCompatibility: [],
        lineageCompatibility: [],
        morphology,
        firstGeneratedSeq: 10_000_000,
        pathsPath: join(directory, 'paths.bin'),
        occurrencesPath: join(directory, 'occurrences.bin')
      });
      const compiled = compileBoundedGeneratedProjection({
        projection: result,
        entries: [root],
        morphology,
        temporaryDirectory: directory,
        customRootSeqs: new Set(),
        firstErrataEvent: 10,
        maxOccurrenceChunkRows: 100
      });
      expect(compiled.generated.matchedPaths).toBe(compiled.generated.semanticPaths + 1);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
