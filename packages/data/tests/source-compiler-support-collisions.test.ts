import { beforeAll, describe, expect, test } from 'bun:test';
import { fileURLToPath } from 'node:url';
import type {
  CompiledMorphologyArtifact,
  CompiledMorphologyRule
} from '../src/browser-pack/morphology-format.js';
import { loadAllConjugationRules } from '../src/data/conj-rules.js';
import { compileAnalyzerSupportCollisions } from '../src/source-compiler/analyzer-support-collisions.js';
import {
  emitCanonicalConjugations,
  type ConjugationEmission,
  type EmissionRule
} from '../src/source-compiler/conjugation-emissions.js';
import {
  assignPhysicalTargets,
  lexicalPhysicalTarget
} from '../src/source-compiler/conjugation-emissions-physical.js';
import type {
  CanonicalEntry,
  CanonicalForm,
  CanonicalSense
} from '../src/source-compiler/model.js';

const RULE_DATA = fileURLToPath(new URL('../../../data', import.meta.url));

function form(text: string, ordinal: number, conjugatable: boolean): CanonicalForm {
  return {
    text,
    ordinal,
    sourceOrder: { event: 0, ordinal },
    common: null,
    priorityTags: [],
    conjugatable,
    noKanji: false,
    best: null
  };
}

function sense(
  ordinal: number,
  properties: readonly [tag: 'pos' | 'misc', text: string][]
): CanonicalSense {
  return {
    ordinal,
    glosses: [],
    properties: properties.map(([tag, text], propertyOrdinal) => ({
      tag,
      text,
      ordinal: propertyOrdinal,
      sourceOrder: { event: 0, ordinal: propertyOrdinal }
    }))
  };
}

function entry(options: {
  readonly seq: number;
  readonly kanji: readonly string[];
  readonly kana: readonly string[];
  readonly senses: readonly CanonicalSense[];
  readonly conjugatable: boolean;
}): CanonicalEntry {
  return {
    seq: options.seq,
    source: { sourceId: 'collision-test', ordinal: options.seq },
    kanji: options.kanji.map((text, ordinal) => form(text, ordinal, options.conjugatable)),
    kana: options.kana.map((text, ordinal) => form(text, ordinal, options.conjugatable)),
    senses: options.senses,
    restrictions: [],
    primaryNoKanji: options.kanji.length === 0
  };
}

function compiledRule(rule: EmissionRule): CompiledMorphologyRule {
  return {
    pos: rule.pos,
    type: rule.type,
    negative: rule.negative,
    formal: rule.formal,
    ordinal: rule.order,
    stem: rule.stem,
    okuri: rule.okuri,
    euphr: rule.euphr,
    euphk: rule.euphk
  };
}

function morphologyArtifact(emissions: readonly ConjugationEmission[]): CompiledMorphologyArtifact {
  const rules = new Map<string, CompiledMorphologyRule>();
  for (const emission of emissions) {
    for (const form of emission.forms) {
      for (const rule of [form.firstRule, form.secondRule]) {
        if (rule === null) continue;
        const compiled = compiledRule(rule);
        rules.set(JSON.stringify(compiled), compiled);
      }
    }
  }
  const ordered = [...rules.values()].sort((left, right) => {
    const leftKey = JSON.stringify(left);
    const rightKey = JSON.stringify(right);
    return leftKey < rightKey ? -1 : leftKey > rightKey ? 1 : 0;
  });
  return {
    positions: [...new Set(ordered.map(rule => rule.pos))].sort(),
    rules: ordered,
    templates: [],
    rootKeys: [],
    rootGroups: [],
    patches: [],
    tombstones: []
  };
}

beforeAll(() => loadAllConjugationRules(RULE_DATA));

describe('source-native analyzer collision facts', () => {
  test('projects direct reuse, secondary lineage, lexical facts and tombstones', () => {
    const root = entry({
      seq: 1_156_870,
      kanji: ['慰む'],
      kana: ['なぐさむ'],
      senses: [sense(0, [['pos', 'v5m']])],
      conjugatable: true
    });
    const potential = entry({
      seq: 1_156_890,
      kanji: ['慰める'],
      kana: ['なぐさめる'],
      senses: [
        sense(0, [['pos', 'v1'], ['misc', 'uk']]),
        sense(1, [['pos', 'prt'], ['misc', 'arch']])
      ],
      conjugatable: false
    });
    const imperative = entry({
      seq: 1_156_880,
      kanji: ['慰め'],
      kana: ['なぐさめ'],
      senses: [sense(0, [['pos', 'exp']])],
      conjugatable: false
    });
    const entries = [root, potential, imperative];
    const emissions = entries.flatMap(emitCanonicalConjugations);
    const physical = assignPhysicalTargets(
      emissions,
      entries.map(lexicalPhysicalTarget),
      10_000_000
    );
    const morphology = morphologyArtifact(emissions);

    const collisions = compileAnalyzerSupportCollisions(entries, emissions, physical, morphology);
    const direct = collisions.filter(value => value.collisionSeq === potential.seq);
    const imperativeCollisions = collisions.filter(value => value.collisionSeq === imperative.seq);
    const secondary = imperativeCollisions.filter(value => value.ruleIds.length === 2);

    expect(collisions).toHaveLength(6);
    expect(direct.map(value => value.surface)).toEqual(['なぐさめる', '慰める']);
    expect(direct.every(value => value.ruleIds.length === 1 && value.viaSeq === null)).toBe(true);
    expect(direct[0]).toEqual(expect.objectContaining({
      nKanji: 1,
      nKana: 1,
      primaryNokanji: false,
      archived: false,
      preferKana: true,
      preferKanaOnOrdinalZero: true,
      pos: ['v1'],
      skipWord: false
    }));

    expect(imperativeCollisions.filter(value => value.ruleIds.length === 1)
      .map(value => value.surface)).toEqual(['なぐさめ', '慰め']);
    expect(secondary.map(value => value.surface)).toEqual(['なぐさめ', '慰め']);
    expect(secondary.every(value =>
      value.ruleIds.length === 2 && value.viaSeq === potential.seq)).toBe(true);
    expect(secondary.every(value => !value.skipWord)).toBe(true);

    const removed = secondary[0]!;
    const tombstoned = compileAnalyzerSupportCollisions(entries, emissions, physical, {
      ...morphology,
      tombstones: [...morphology.tombstones, {
        route: removed.route,
        surface: removed.surface,
        rootSeq: removed.rootSeq,
        firstRule: removed.ruleIds[0],
        secondRule: removed.ruleIds[1] ?? null
      }]
    });
    expect(tombstoned).toHaveLength(collisions.length - 1);
    expect(tombstoned.some(value =>
      value.route === removed.route
      && value.surface === removed.surface
      && value.ruleIds[0] === removed.ruleIds[0]
      && value.ruleIds[1] === removed.ruleIds[1])).toBe(false);
  });

  test('is independent of canonical and emission input order', () => {
    const root = entry({
      seq: 2_089_020,
      kanji: [],
      kana: ['だ'],
      senses: [sense(0, [['pos', 'cop-da']])],
      conjugatable: true
    });
    const desu = entry({
      seq: 1_628_500,
      kanji: [],
      kana: ['です'],
      senses: [sense(0, [['pos', 'exp']])],
      conjugatable: false
    });
    const entries = [root, desu];
    const emissions = entries.flatMap(emitCanonicalConjugations);
    const morphology = morphologyArtifact(emissions);
    const physical = assignPhysicalTargets(
      emissions,
      entries.map(lexicalPhysicalTarget),
      10_000_000
    );
    const expected = compileAnalyzerSupportCollisions(entries, emissions, physical, morphology);
    const reversedEmissions = [...emissions].reverse();
    const reversedPhysical = assignPhysicalTargets(
      reversedEmissions,
      [...entries].reverse().map(lexicalPhysicalTarget),
      10_000_000
    );

    expect(compileAnalyzerSupportCollisions(
      [...entries].reverse(),
      reversedEmissions,
      reversedPhysical,
      morphology
    )).toEqual(expected);
    expect(expected.filter(value => value.surface === 'です')).toEqual([
      expect.objectContaining({
        rootSeq: root.seq,
        collisionSeq: desu.seq,
        viaSeq: null,
        route: 'kana'
      })
    ]);
  });
});
