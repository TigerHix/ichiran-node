import { describe, expect, test } from 'bun:test';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { buildMorphology } from '../src/browser-pack/morphology-compiler.js';
import { compileBoundedGeneratedProjection } from '../src/source-compiler/analyzer-generated-stream.js';
import { conjugationPositionsByRoot } from '../src/source-compiler/conjugation-emission-order.js';
import { writeScheduledGeneratedProjection } from '../src/source-compiler/generated-projection-stream.js';
import {
  reduceGeneratedOccurrenceSurfaces,
  reduceGeneratedSemanticPaths
} from '../src/source-compiler/generated-projection-reduce.js';
import type { CanonicalEntry } from '../src/source-compiler/model.js';

const dataPath = fileURLToPath(new URL('../../../data', import.meta.url));

function entry(): CanonicalEntry {
  return {
    seq: 1_519_210,
    source: { id: 'fixture', ordinal: 0 },
    kanji: [{
      text: '忘れる', ordinal: 0, sourceOrder: { event: 0, ordinal: 0 },
      common: null, priorityTags: [], conjugatable: true, best: 'わすれる'
    }],
    kana: [{
      text: 'わすれる', ordinal: 0, sourceOrder: { event: 0, ordinal: 0 },
      common: null, priorityTags: [], conjugatable: true, noKanji: false, best: '忘れる'
    }],
    senses: [{ ordinal: 0, properties: [], glosses: [{ text: 'forget', ordinal: 0 }] }]
  };
}

describe('bounded generated projection producer', () => {
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
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
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
