import { beforeAll, describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import type {
  AnalyzerSupportSplitPartSource
} from '../src/browser-pack/analyzer-support.js';
import type { CompiledMorphologyArtifact } from '../src/browser-pack/morphology-format.js';
import { loadAllConjugationRules } from '../src/data/conj-rules.js';
import {
  compileAnalyzerSupportAnnotations
} from '../src/source-compiler/analyzer-support-annotations.js';
import {
  LEGACY_EASY_HINT_DECLARATIONS,
  UPSTREAM_260118_EASY_HINT_DECLARATIONS
} from '../src/source-compiler/analyzer-support-easy-hint-declarations.js';
import {
  LEGACY_SIMPLE_HINT_DECLARATIONS,
  UPSTREAM_260118_SIMPLE_HINT_DECLARATIONS
} from '../src/source-compiler/analyzer-support-simple-hint-declarations.js';
import {
  LEGACY_SPLIT_DECLARATIONS,
  SEGMENT_SPLIT_DECLARATIONS,
  SPLIT_DECLARATIONS
} from '../src/source-compiler/analyzer-support-split-declarations.js';
import {
  createBoundedSourceNativeSplitPartResolver,
  createSourceNativeSplitPartResolver
} from '../src/source-compiler/analyzer-support-split-resolver.js';
import {
  emitCanonicalConjugations
} from '../src/source-compiler/conjugation-emissions.js';
import {
  assignPhysicalTargets,
  lexicalPhysicalTarget
} from '../src/source-compiler/conjugation-emissions-physical.js';
import { GeneratedProjectionSpoolWriter } from '../src/source-compiler/generated-projection-spool.js';
import type { GeneratedProjectionStreamResult } from '../src/source-compiler/generated-projection-stream.js';
import {
  loadKanjidicHintReadings
} from '../src/source-compiler/kanjidic-hints.js';
import type {
  CanonicalEntry,
  CanonicalForm,
  CanonicalSense
} from '../src/source-compiler/model.js';

const DATA = fileURLToPath(new URL('../../../data', import.meta.url));
const KANJIDIC = fileURLToPath(new URL('../kanjidic2.xml.gz', import.meta.url));
const EMPTY_MORPHOLOGY: CompiledMorphologyArtifact = {
  positions: [],
  rules: [],
  templates: [],
  rootKeys: [],
  rootGroups: [],
  patches: [],
  tombstones: []
};

function form(
  text: string,
  ordinal: number,
  best: string | null = null,
  sourceEvent = 0
): CanonicalForm {
  return {
    text,
    ordinal,
    sourceOrder: { event: sourceEvent, ordinal },
    common: null,
    priorityTags: [],
    conjugatable: true,
    noKanji: false,
    best
  };
}

function entry(
  seq: number,
  kanji: readonly CanonicalForm[],
  kana: readonly CanonicalForm[],
  position = 'exp'
): CanonicalEntry {
  const sense: CanonicalSense = {
    ordinal: 0,
    glosses: [],
    properties: [{
      tag: 'pos',
      text: position,
      ordinal: 0,
      sourceOrder: { event: 0, ordinal: 0 }
    }]
  };
  return {
    seq,
    source: { sourceId: 'annotation-test', ordinal: seq },
    kanji,
    kana,
    senses: [sense],
    restrictions: [],
    primaryNoKanji: kanji.length === 0
  };
}

function part(seq: number, text: string): AnalyzerSupportSplitPartSource {
  return {
    seq,
    route: 'kana',
    text,
    best: null,
    ord: 0,
    common: null,
    commonTags: '',
    conjugatable: false,
    nokanji: true,
    generated: null
  };
}

function digest(values: readonly number[]): string {
  return createHash('sha256')
    .update([...values].sort((left, right) => left - right).join('\n') + '\n')
    .digest('hex');
}

beforeAll(() => loadAllConjugationRules(DATA));

describe('source-native split and hint declarations', () => {
  test('pins the complete qualified declaration identities', () => {
    const legacyHints = [
      ...LEGACY_SIMPLE_HINT_DECLARATIONS.map(value => value[0]),
      ...LEGACY_EASY_HINT_DECLARATIONS.map(value => value[0])
    ];
    const hints = [
      ...legacyHints,
      ...UPSTREAM_260118_SIMPLE_HINT_DECLARATIONS.map(value => value[0]),
      ...UPSTREAM_260118_EASY_HINT_DECLARATIONS.map(value => value[0])
    ];

    expect(LEGACY_SPLIT_DECLARATIONS).toHaveLength(172);
    expect(SPLIT_DECLARATIONS).toHaveLength(174);
    expect(SEGMENT_SPLIT_DECLARATIONS).toHaveLength(18);
    expect(legacyHints).toHaveLength(645);
    expect(hints).toHaveLength(658);
    expect(new Set(hints).size).toBe(658);
    expect(digest(LEGACY_SPLIT_DECLARATIONS.map(value => value.seq)))
      .toBe('896f42769d22c43d95f24d2001e5c097957c53d24ff38630727048676dd8b202');
    expect(digest(SPLIT_DECLARATIONS.map(value => value.seq)))
      .toBe('f05314aaa8ba56ae9293c4deebdd1580f957f879b813d8a219f6b8bc67bc0049');
    expect(digest(SEGMENT_SPLIT_DECLARATIONS.map(value => value.seq)))
      .toBe('3ae821a6049874e6b59ed3e28d2e5395b52c8f37cbf076b845b417cca98af0a3');
    expect(digest(legacyHints))
      .toBe('62cd88b14d37a0cb340324eed53643cdc5c1de38ce03a0e06f59c04c25199e9d');
    expect(digest(hints))
      .toBe('91c41f4060ebc6619e2de3069bc6fe270cb55e2d7677acfdadefab79f67ffde2');
  });

  test('evaluates current split and segment-split rules over direct semantic forms', async () => {
    const readings = await loadKanjidicHintReadings(KANJIDIC);
    const entries = [
      entry(1_774_820, [], [form('からすき', 0)]),
      entry(1_208_870, [], [form('かなって', 0), form('かなった', 1)]),
      entry(2_771_940, [], [form('はないです', 0)])
    ];
    const parts = new Map([
      [JSON.stringify([1_002_980, 'から']), part(1_002_980, 'から')],
      [JSON.stringify([1_277_450, 'すき']), part(1_277_450, 'すき')],
      [JSON.stringify([1_002_940, 'かな']), part(1_002_940, 'かな')],
      [JSON.stringify([2_086_960, 'って']), part(2_086_960, 'って')]
    ]);
    const compiled = compileAnalyzerSupportAnnotations({
      entries,
      morphology: EMPTY_MORPHOLOGY,
      collisions: [],
      partResolver: {
        find(text, seqs) {
          for (const seq of seqs) {
            const value = parts.get(JSON.stringify([seq, text]));
            if (value) return value;
          }
          return null;
        }
      },
      kanjidicReadings: readings
    });

    expect(compiled.splits).toEqual([
      expect.objectContaining({
        definitionSeq: 1_208_870,
        kind: 'segsplit',
        surface: 'かなって',
        score: 5,
        parts: [part(1_002_940, 'かな'), part(2_086_960, 'って')]
      }),
      expect.objectContaining({
        definitionSeq: 1_774_820,
        kind: 'split',
        surface: 'からすき',
        score: -5,
        parts: [part(1_002_980, 'から'), part(1_277_450, 'すき')]
      }),
      expect.objectContaining({
        definitionSeq: 2_771_940,
        kind: 'split',
        surface: 'はないです',
        score: -5,
        parts: []
      })
    ]);
  });

  test('compiles both simple and Kanjidic-backed easy hints', async () => {
    const readings = await loadKanjidicHintReadings(KANJIDIC);
    const compiled = compileAnalyzerSupportAnnotations({
      entries: [
        entry(2_028_920, [], [form('は', 0)]),
        entry(
          2_140_350,
          [form('時は金なり', 0, 'ときはかねなり')],
          [form('ときはかねなり', 0, '時は金なり')]
        )
      ],
      morphology: EMPTY_MORPHOLOGY,
      collisions: [],
      partResolver: { find: () => null },
      kanjidicReadings: readings
    });

    expect(compiled.hints).toContainEqual({
      definitionSeq: 2_028_920,
      route: 'kana',
      surface: 'は',
      reading: 'は',
      hint: '\u200cは'
    });
    expect(compiled.hints).toContainEqual({
      definitionSeq: 2_140_350,
      route: 'kanji',
      surface: '時は金なり',
      reading: 'ときはかねなり',
      hint: 'とき\u200b\u200cは\u200bかね\u200bなり'
    });
  });

  test('resolves a generated split part by ancestor and semantic lineage', () => {
    const root = entry(1_589_040, [form('遅れる', 0, 'おくれる')], [form('おくれる', 0, '遅れる')], 'v1');
    const past = emitCanonicalConjugations(root).find(emission =>
      emission.stage === 'primary'
      && emission.first.type === 2
      && emission.first.negative === false
      && emission.first.formal === false)!;
    const rule = past.forms[0]!.firstRule;
    const morphology: CompiledMorphologyArtifact = {
      positions: ['v1'],
      rules: [{
        pos: rule.pos,
        type: rule.type,
        negative: rule.negative,
        formal: rule.formal,
        ordinal: rule.order,
        stem: rule.stem,
        okuri: rule.okuri,
        euphr: rule.euphr,
        euphk: rule.euphk
      }],
      templates: [{ suffix: 'た', removed: 'る', firstRule: 0, secondRule: null }],
      rootKeys: [{
        route: 'kana',
        pos: 'v1',
        sourceText: 'おくれる',
        records: [{
          rootGroup: 0,
          sourceForm: '遅れる',
          sourceReading: 'おくれる',
          ord: 0,
          common: null
        }]
      }],
      rootGroups: [{ seq: 1_589_040, forms: ['遅れる', 'おくれる'] }],
      patches: [],
      tombstones: []
    };
    const physical = assignPhysicalTargets([past], [lexicalPhysicalTarget(root)], 3_000_000);
    const resolver = createSourceNativeSplitPartResolver({
      entries: [root],
      morphology,
      emissions: [past],
      physical
    });
    const resolved = resolver.find('おくれた', [1_589_040], true);

    expect(resolved).toEqual(expect.objectContaining({
      seq: expect.any(Number),
      route: 'kana',
      text: 'おくれた',
      best: null,
      ord: 0,
      generated: expect.arrayContaining([
        expect.objectContaining({ from: 1_589_040, via: false, pos: 'v1', type: 2 })
      ])
    }));
    expect(typeof resolved === 'string' ? 0 : resolved?.seq).toBeGreaterThanOrEqual(3_000_000);
  });

  test('resolves competing direct parts in canonical source order', () => {
    const later = entry(1_590_770, [], [form('かわり', 0, '代わり', 20)]);
    const earlier = entry(1_510_720, [], [form('かわり', 0, '変わり', 10)]);
    const physical = assignPhysicalTargets(
      [],
      [lexicalPhysicalTarget(later), lexicalPhysicalTarget(earlier)],
      3_000_000
    );
    const resolver = createSourceNativeSplitPartResolver({
      entries: [later, earlier],
      morphology: EMPTY_MORPHOLOGY,
      emissions: [],
      physical
    });

    expect(resolver.find('かわり', [1_590_770, 1_510_720], false))
      .toEqual(expect.objectContaining({ seq: 1_510_720, best: '変わり' }));
  });

  test('retains lineage for a declared lexical target', () => {
    const directory = mkdtempSync(join(tmpdir(), 'ichiran-split-locator-'));
    try {
      const pathsPath = join(directory, 'paths.bin');
      const occurrencesPath = join(directory, 'occurrences.bin');
      const writer = new GeneratedProjectionSpoolWriter(pathsPath, occurrencesPath);
      writer.writePath({
        ordinal: 0,
        rootSeq: 1_405_790,
        firstAlias: 0,
        secondAlias: null,
        targetSeq: 1_405_800,
        viaTargetSeq: null
      });
      writer.close();
      const target = entry(
        1_405_800,
        [form('続ける', 0, 'つづける')],
        [form('つづける', 0, '続ける')]
      );
      const resolver = createBoundedSourceNativeSplitPartResolver({
        entries: [target],
        morphology: EMPTY_MORPHOLOGY,
        projection: {
          pathsPath,
          occurrencesPath,
          targets: [{
            seq: target.seq,
            kanji: ['続ける'],
            kana: ['つづける'],
            secondaryForms: [],
            conjugatable: true,
            origin: 'lexical'
          }],
          aliasProperties: [{
            pos: 'v5k', type: 5, negative: false, formal: false
          }]
        } as GeneratedProjectionStreamResult
      });

      expect(resolver.find('つづける', [1_405_800], false))
        .toEqual(expect.objectContaining({
          seq: 1_405_800,
          generated: [{
            from: 1_405_790,
            via: false,
            pos: 'v5k',
            type: 5,
            negative: false,
            formal: false
          }]
        }));
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
