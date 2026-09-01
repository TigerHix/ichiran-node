import { beforeAll, describe, expect, test } from 'bun:test';
import { fileURLToPath } from 'node:url';
import { buildAnalyzerAnnotations } from '../src/browser-pack/analyzer-annotations.js';
import type {
  CompiledMorphologyArtifact,
  CompiledMorphologyRule
} from '../src/browser-pack/morphology-format.js';
import { loadAllConjugationRules } from '../src/data/conj-rules.js';
import { compileSourceNativeGeneratedInput } from '../src/source-compiler/analyzer-generated-input.js';
import { lookupClassKey } from '../src/source-compiler/analyzer-generated-order.js';
import {
  denseEmissionPrecedence,
  emitConfiguredConjugations
} from '../src/source-compiler/conjugation-emission-order.js';
import { scheduleSourceNativeConjugations } from '../src/source-compiler/conjugation-scheduler.js';
import {
  conjugationEmissionKey,
  conjugationSourceKey,
  emitPrimaryConjugations,
  type ConjugationEmission,
  type EmissionRule
} from '../src/source-compiler/conjugation-emissions.js';
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

function entry(
  seq: number,
  kanji: readonly string[],
  kana: readonly string[],
  positions: readonly string[],
  conjugatable: boolean
): CanonicalEntry {
  const sense: CanonicalSense = {
    ordinal: 0,
    glosses: [],
    properties: positions.map((text, ordinal) => ({
      tag: 'pos',
      text,
      ordinal,
      sourceOrder: { event: 0, ordinal }
    }))
  };
  return {
    seq,
    source: { sourceId: 'generated-support-test', ordinal: seq },
    kanji: kanji.map((text, ordinal) => form(text, ordinal, conjugatable)),
    kana: kana.map((text, ordinal) => form(text, ordinal, conjugatable)),
    senses: [sense],
    restrictions: [],
    primaryNoKanji: kanji.length === 0
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
  const byKey = new Map<string, CompiledMorphologyRule>();
  for (const emission of emissions) {
    for (const generated of emission.forms) {
      for (const rule of [generated.firstRule, generated.secondRule]) {
        if (rule === null) continue;
        const value = compiledRule(rule);
        byKey.set(JSON.stringify(value), value);
      }
    }
  }
  const rules = [...byKey.values()].sort((left, right) => {
    const leftKey = JSON.stringify(left);
    const rightKey = JSON.stringify(right);
    return leftKey < rightKey ? -1 : leftKey > rightKey ? 1 : 0;
  });
  return {
    positions: [...new Set(rules.map(rule => rule.pos))].sort(),
    rules,
    templates: [],
    rootKeys: [],
    rootGroups: [],
    patches: [],
    tombstones: []
  };
}

function morphologyWithRoots(
  morphology: CompiledMorphologyArtifact,
  roots: readonly { readonly entry: CanonicalEntry; readonly pos: string }[]
): CompiledMorphologyArtifact {
  const ordered = [...roots].sort((left, right) => left.entry.seq - right.entry.seq);
  return {
    ...morphology,
    rootGroups: ordered.map(value => ({
      seq: value.entry.seq,
      forms: [...value.entry.kanji, ...value.entry.kana].map(form => form.text).sort()
    })),
    rootKeys: ordered.flatMap((value, rootGroup) => [
      ...value.entry.kana.map(form => ({
        route: 'kana' as const,
        pos: value.pos,
        sourceText: form.text,
        records: [{
          rootGroup,
          sourceForm: form.best ?? form.text,
          sourceReading: form.text,
          ord: form.ordinal,
          common: form.common
        }]
      })),
      ...value.entry.kanji.map(form => ({
        route: 'kanji' as const,
        pos: value.pos,
        sourceText: form.text,
        records: [{
          rootGroup,
          sourceForm: form.text,
          sourceReading: form.best ?? form.text,
          ord: form.ordinal,
          common: form.common
        }]
      }))
    ])
  };
}

function property(emissions: readonly ConjugationEmission[], type: number): ConjugationEmission {
  const emission = emissions.find(value => value.first.type === type);
  if (!emission) throw new Error(`Missing type ${type}`);
  return emission;
}

function selection(entry: CanonicalEntry, positions: readonly string[]) {
  const sources = new Set([
    ...entry.kanji.map(value => conjugationSourceKey('kanji', value.text)),
    ...entry.kana.map(value => conjugationSourceKey('kana', value.text))
  ]);
  return {
    positions,
    sourcesByPosition: new Map(positions.map(pos => [pos, sources]))
  };
}

beforeAll(() => loadAllConjugationRules(RULE_DATA));

describe('source-native generated analyzer projection', () => {
  test('projects aliases, physical cross-products, counts and exact direct/generated order', () => {
    const root = entry(1_358_280, ['食べる'], ['たべる'], ['v1'], true);
    // Shares the written collision but is not a compatible physical target:
    // its reading differs from the generated target's required reading.
    const lexical = entry(900_001, ['食べられる'], ['しょくべられる'], ['exp'], false);
    const primary = emitPrimaryConjugations(root, { types: new Set([5, 6]) });
    const emissions = [property(primary, 5), property(primary, 6)];
    const precedence = denseEmissionPrecedence(emissions);
    const morphology = morphologyArtifact(emissions);
    const generatedTarget = 10_000_000;
    const build = compileSourceNativeGeneratedInput({
      entries: [root, lexical],
      emissions,
      positionsByRoot: new Map([[root.seq, ['v1']]]),
      emissionPrecedence: precedence,
      lookupClassPrecedence: new Map([
        [lookupClassKey('kanji', '食べられる', lexical.seq), 20],
        [lookupClassKey('kanji', '食べられる', generatedTarget), 10]
      ]),
      firstGeneratedSeq: generatedTarget,
      morphology
    });

    expect(build.physical.targets.filter(value => value.origin === 'generated')).toHaveLength(1);
    expect(new Set(build.physical.bindings.map(value => value.targetSeq))).toEqual(
      new Set([generatedTarget])
    );
    expect(build.generated).toEqual(expect.objectContaining({
      aliasCount: 2,
      semanticPaths: 2,
      matchedPaths: 2,
      countExceptions: 2,
      physicalGroups: 1,
      physicalMembers: 4,
      propertyOverrides: 2,
      maxMemberOrd: 0,
      maxViaMemberOrd: 0,
      maxPropOrd: 1,
      lookupOrderSurfaces: 1,
      lookupOrderClasses: 2,
      lookupOrderExceptionClasses: 0,
      lookupOrderExceptionLocators: 0
    }));
    expect(build.generated.records).toHaveLength(2);
    expect(build.generated.records.every(value =>
      value.counts?.[0] === 2
      && value.counts[1] === 2
      && value.physicalGroup === 1
      && value.members?.length === 2)).toBe(true);
    expect(build.generated.lookupOrderExceptions).toEqual([]);
    expect(build.generated.lookupOrders).toEqual(expect.arrayContaining([
      { rootSeq: lexical.seq, firstAlias: null, secondAlias: null, rank: 0 },
      expect.objectContaining({ rootSeq: root.seq, rank: 1 })
    ]));

    // The existing qualified pack writer accepts compiler-owned semantic input.
    const encoded = buildAnalyzerAnnotations([], [], build.generated);
    expect(encoded.bytes.byteLength).toBeGreaterThan(0);
    expect(encoded.stats.generatedRecords).toBe(4);
  });

  test('requires the source-owned position selection and works with PostgreSQL unavailable', () => {
    const ambiguous = entry(999_001, ['そうだ'], ['そうだ'], ['cop', 'v1'], true);
    const configured = emitConfiguredConjugations(ambiguous, {
      positions: ['v1'],
      sourcesByPosition: new Map([[
        'v1',
        new Set([conjugationSourceKey('kana', 'そうだ')])
      ]])
    });
    expect(configured.length).toBeGreaterThan(0);
    expect(configured.every(value => value.first.pos === 'v1')).toBe(true);
    expect(configured.flatMap(value => value.forms).every(value => value.route === 'kana')).toBe(true);

    const morphology = morphologyArtifact(configured);
    const prior = process.env.DATABASE_URL;
    process.env.DATABASE_URL = 'postgresql://127.0.0.1:1/postgres';
    try {
      expect(() => compileSourceNativeGeneratedInput({
        entries: [ambiguous],
        emissions: configured,
        positionsByRoot: new Map(),
        emissionPrecedence: denseEmissionPrecedence(configured),
        lookupClassPrecedence: new Map(),
        firstGeneratedSeq: 10_000_000,
        morphology
      })).toThrow('unconfigured position v1');
    } finally {
      if (prior === undefined) delete process.env.DATABASE_URL;
      else process.env.DATABASE_URL = prior;
    }
  });

  test('schedules base, custom and chronological conjugations in global phases', () => {
    const base = entry(700_001, ['決める'], ['きめる'], ['v1'], true);
    const custom = entry(700_002, ['止める'], ['とめる'], ['v1'], true);
    const da = entry(2_089_020, [], ['だ'], ['cop-da'], true);
    const all = [
      ...emitConfiguredConjugations(base, selection(base, ['v1'])),
      ...emitConfiguredConjugations(custom, selection(custom, ['v1'])),
      ...emitConfiguredConjugations(da, selection(da, ['cop']))
    ];
    const scheduled = scheduleSourceNativeConjugations({
      entries: [custom, da, base],
      positionsByRoot: new Map([
        [base.seq, ['v1']],
        [custom.seq, ['v1']],
        [da.seq, ['cop']]
      ]),
      customRootSeqs: new Set([custom.seq]),
      firstErrataEvent: 50,
      chronologicalPositions: [{ rootSeq: da.seq, pos: 'cop', event: 50 }],
      suppressions: [],
      lineageCompatibility: [],
      morphology: morphologyWithRoots(morphologyArtifact(all), [
        { entry: base, pos: 'v1' },
        { entry: custom, pos: 'v1' },
        { entry: da, pos: 'cop' }
      ])
    });
    const phases = new Map<number, Set<number>>();
    for (const emission of scheduled.emissions) {
      const phase = scheduled.creationByEmission.get(conjugationEmissionKey(emission))?.[0];
      if (phase === undefined) throw new Error('Scheduled emission has no phase');
      const values = phases.get(emission.rootSeq) ?? new Set<number>();
      values.add(phase);
      phases.set(emission.rootSeq, values);
    }
    expect(phases.get(base.seq)).toEqual(new Set([1, 2]));
    expect(phases.get(custom.seq)).toEqual(new Set([4, 5]));
    expect(phases.get(da.seq)).toEqual(new Set([6]));
    expect(scheduled.precedence.size).toBe(scheduled.emissions.length);
  });
});
