import { beforeAll, describe, expect, test } from 'bun:test';
import { fileURLToPath } from 'node:url';
import { loadAllConjugationRules } from '../src/data/conj-rules.js';
import {
  conjugationEmissionKey,
  conjugationSourceKey,
  emitCanonicalConjugations,
  emitPrimaryConjugations,
  emitSecondaryConjugations,
  summarizeConjugationRelation,
  type ConjugationEmission,
  type EmissionForm,
  type EmissionRule
} from '../src/source-compiler/conjugation-emissions.js';
import {
  assignPhysicalTargets,
  StreamingPhysicalTargetAllocator,
  type PhysicalTarget
} from '../src/source-compiler/conjugation-emissions-physical.js';
import type {
  CanonicalEntry,
  CanonicalForm,
  CanonicalSense
} from '../src/source-compiler/model.js';

function form(text: string, ordinal: number, conjugatable = true): CanonicalForm {
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
  position: string,
  nonConjugatingKana: ReadonlySet<string> = new Set()
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
    source: { sourceId: 'qualified-witness', ordinal: seq },
    kanji: kanji.map((text, ordinal) => form(text, ordinal)),
    kana: kana.map((text, ordinal) => form(text, ordinal, !nonConjugatingKana.has(text))),
    senses: [sense],
    restrictions: [],
    primaryNoKanji: kanji.length === 0
  };
}

function property(
  emissions: readonly ConjugationEmission[],
  type: number,
  negative: boolean | null,
  formal: boolean | null
): ConjugationEmission {
  const value = emissions.find(emission => {
    const final = emission.second ?? emission.first;
    return final.type === type && final.negative === negative && final.formal === formal;
  });
  if (!value) throw new Error(`Missing conjugation ${type}/${String(negative)}/${String(formal)}`);
  return value;
}

function surfaces(emission: ConjugationEmission): string[] {
  return emission.forms.map(form => form.surface);
}

function physicalPrimary(
  rootSeq: number,
  route: 'kanji' | 'kana',
  surface: string
): ConjugationEmission {
  const rule: EmissionRule = {
    pos: 'v5s',
    type: 53,
    negative: false,
    formal: false,
    order: 1,
    stem: 1,
    okuri: 'さす',
    euphr: '',
    euphk: ''
  };
  const form: EmissionForm = {
    route,
    surface,
    sourceText: surface,
    sourceEvent: 0,
    sourceOrdinal: 0,
    secondaryEligible: true,
    physicalCounterpart: null,
    intermediate: null,
    firstRule: rule,
    secondRule: null
  };
  return {
    rootSeq,
    rootEvent: 0,
    stage: 'primary',
    ordinal: 0,
    first: rule,
    second: null,
    via: null,
    physicalForms: [form],
    forms: [form]
  };
}

function pairedPhysicalPrimary(
  rootSeq: number,
  forms: readonly {
    readonly route: 'kana' | 'kanji';
    readonly surface: string;
    readonly counterpart: string | null;
  }[]
): ConjugationEmission {
  const seed = physicalPrimary(rootSeq, forms[0]!.route, forms[0]!.surface);
  const physicalForms = forms.map((value, sourceOrdinal) => ({
    ...seed.physicalForms[0]!,
    route: value.route,
    surface: value.surface,
    sourceText: value.surface,
    sourceOrdinal,
    physicalCounterpart: value.counterpart
  }));
  return { ...seed, physicalForms, forms: physicalForms };
}

const RULE_DATA = fileURLToPath(new URL('../../../data', import.meta.url));

beforeAll(() => loadAllConjugationRules(RULE_DATA));

describe('source-native forward conjugation emissions', () => {
  test('1519210 忘れる has ordered direct past lineage', () => {
    const wasureru = entry(1519210, ['忘れる'], ['わすれる'], 'v1');
    const primary = emitPrimaryConjugations(wasureru);
    const past = property(primary, 2, false, false);

    expect(surfaces(past)).toEqual(['忘れた', 'わすれた']);
    expect(past.stage).toBe('primary');
    expect(past.via).toBeNull();
    expect(past.forms.map(form => [form.sourceText, form.intermediate])).toEqual([
      ['忘れる', null],
      ['わすれる', null]
    ]);
  });

  test('1337800 熟す chains only through its explicit causative-su emission', () => {
    const jukusu = entry(1337800, ['熟す'], ['じゅくす'], 'v5s');
    const primary = emitPrimaryConjugations(jukusu);
    const causativeSu = property(primary, 53, false, false);
    expect(surfaces(causativeSu)).toEqual(['熟さす', 'じゅくさす']);

    const secondary = emitSecondaryConjugations(causativeSu);
    const command = property(secondary, 10, false, true);
    expect(surfaces(command)).toEqual(['熟さしなさい', 'じゅくさしなさい']);
    expect(command.via).toBe(conjugationEmissionKey(causativeSu));
    expect(command.forms.map(form => form.intermediate)).toEqual(['熟さす', 'じゅくさす']);

    const past = property(primary, 2, false, false);
    expect(emitSecondaryConjugations(past)).toEqual([]);
  });

  test('1358280 食べる keeps semantic candidates separate from physical cross-products', () => {
    const taberu = entry(1358280, ['食べる', '喰べる'], ['たべる'], 'v1');
    const primary = emitPrimaryConjugations(taberu, { types: new Set([5, 6]) });
    const potential = property(primary, 5, false, false);
    const passive = property(primary, 6, false, false);

    expect(surfaces(potential)).toEqual([
      '食べられる', '食べれる', '喰べられる', '喰べれる', 'たべられる', 'たべれる'
    ]);
    expect(surfaces(passive)).toEqual(['食べられる', '喰べられる', 'たべられる']);
    expect(passive.forms.some(form => form.surface === '食べれる')).toBe(false);

    const physical = assignPhysicalTargets([potential, passive], [], 10_000_000);
    expect(physical.targets).toHaveLength(1);
    expect(physical.targets[0]).toEqual(expect.objectContaining({
      kanji: ['食べられる', '食べれる', '喰べられる', '喰べれる'],
      kana: ['たべられる', 'たべれる']
    }));
    expect(new Set(physical.bindings.map(binding => binding.targetSeq)).size).toBe(1);
    expect(physical.members).toHaveLength(1);
    expect(physical.properties).toHaveLength(2);
    expect(primary.filter(emission => emission.forms.some(form => form.surface === '食べれる')))
      .toEqual([potential]);
  });

  test('keeps the physical reading matrix separate from installed route/source forms', () => {
    const taberu = entry(1358280, ['食べる'], ['たべる'], 'v1');
    const primary = emitPrimaryConjugations(taberu, {
      positions: ['v1'],
      sourcesByPosition: new Map([[
        'v1', new Set([conjugationSourceKey('kanji', '食べる')])
      ]])
    });
    const past = property(primary, 2, false, false);
    expect(past.forms.map(value => value.surface)).toEqual(['食べた']);
    expect(past.physicalForms.map(value => value.surface)).toEqual(['食べた', 'たべた']);

    const secondary = property(
      emitSecondaryConjugations(property(primary, 5, false, false), {
        types: new Set([2]), enforceSurfaceRoute: true
      }),
      2,
      false,
      false
    );
    expect(secondary.forms.map(value => value.surface)).toEqual(['食べられた', '食べれた']);
    expect(secondary.physicalForms.map(value => value.surface)).toEqual([
      '食べられた', '食べれた', 'たべられた', 'たべれた'
    ]);
  });

  test('secondary physical expansion replays every concrete rule declaration', () => {
    const primary = physicalPrimary(9000001, 'kana', 'はらす');
    const negative = property(emitSecondaryConjugations(primary), 3, true, false);
    const source: PhysicalTarget = {
      seq: 8000001,
      kanji: [],
      kana: ['はらす', 'ハラス'],
      secondaryForms: [
        { route: 'kana', text: 'はらす', counterpart: null },
        { route: 'kana', text: 'ハラス', counterpart: 'はらす' }
      ],
      conjugatable: true,
      origin: 'lexical'
    };
    const allocator = new StreamingPhysicalTargetAllocator([source], 10_000_000, []);
    allocator.add({
      ordinal: 0,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 0,
      emission: primary
    });

    const expanded = allocator.expandSecondary(negative, primary.rootSeq, 1);
    expect(expanded.physicalForms.filter(form => form.sourceText === 'ハラス')
      .map(form => form.surface)).toEqual(['ハラさなくて', 'ハラさないで']);
  });

  test('secondary physical expansion rejects a target with no conjugatable kana row', () => {
    const primary = physicalPrimary(9000002, 'kanji', '逸さす');
    const past = property(emitSecondaryConjugations(primary), 2, false, false);
    const source: PhysicalTarget = {
      seq: 8000002,
      kanji: ['逸さす', '佚さす'],
      kana: [],
      secondaryForms: [
        { route: 'kanji', text: '逸さす', counterpart: null },
        { route: 'kanji', text: '佚さす', counterpart: null }
      ],
      conjugatable: true,
      origin: 'lexical'
    };
    const allocator = new StreamingPhysicalTargetAllocator([source], 10_000_000, []);
    allocator.add({
      ordinal: 0,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 0,
      emission: primary
    });

    const physicalOnly = {
      ...past.physicalForms[0]!,
      surface: '佚さした',
      sourceText: '佚さす'
    };
    const expanded = allocator.expandSecondary({
      ...past,
      physicalForms: [...past.physicalForms, physicalOnly]
    }, primary.rootSeq, 1);
    expect(expanded.physicalForms.map(form => form.surface)).toEqual(['逸さした']);
    expect(expanded.physicalForms.some(form => form.surface.startsWith('佚'))).toBe(false);
  });

  test('secondary physical expansion follows the current root reading pair without transitive spill', () => {
    const soku = pairedPhysicalPrimary(9000003, [
      { route: 'kanji', surface: '即さす', counterpart: 'そくさす' },
      { route: 'kana', surface: 'そくさす', counterpart: '即さす' }
    ]);
    const sokuPast = property(emitSecondaryConjugations(soku), 2, false, false);
    const sokuCreator = pairedPhysicalPrimary(9000005, [
      { route: 'kanji', surface: '即さす', counterpart: 'そくさす' },
      { route: 'kanji', surface: '則さす', counterpart: 'そくさす' },
      { route: 'kana', surface: 'そくさす', counterpart: '即さす' }
    ]);
    const sokuAllocator = new StreamingPhysicalTargetAllocator([], 10_000_000, []);
    sokuAllocator.add({
      ordinal: 0,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 0,
      emission: sokuCreator
    });
    sokuAllocator.add({
      ordinal: 1,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 1,
      emission: soku
    });
    const sokuExpanded = sokuAllocator.expandSecondary(sokuPast, soku.rootSeq, 1);
    expect(sokuExpanded.physicalForms.some(form => form.surface === '則さした')).toBe(true);

    const ju = pairedPhysicalPrimary(9000004, [
      { route: 'kanji', surface: '誦さす', counterpart: 'じゅさす' },
      { route: 'kana', surface: 'じゅさす', counterpart: '誦さす' },
      { route: 'kana', surface: 'ずさす', counterpart: '誦さす' }
    ]);
    const juPast = property(emitSecondaryConjugations(ju), 2, false, false);
    const juCreator = pairedPhysicalPrimary(9000006, [
      { route: 'kanji', surface: '誦さす', counterpart: 'しょうさす' },
      { route: 'kanji', surface: '唱さす', counterpart: 'しょうさす' },
      { route: 'kana', surface: 'しょうさす', counterpart: '誦さす' },
      { route: 'kana', surface: 'じゅさす', counterpart: '誦さす' },
      { route: 'kana', surface: 'ずさす', counterpart: '誦さす' }
    ]);
    const juAllocator = new StreamingPhysicalTargetAllocator([], 10_000_000, []);
    juAllocator.add({
      ordinal: 0,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 0,
      emission: juCreator
    });
    juAllocator.add({
      ordinal: 1,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 1,
      emission: ju
    });
    const juExpanded = juAllocator.expandSecondary(juPast, ju.rootSeq, 1);
    expect(juExpanded.physicalForms.some(form => form.surface === 'しょうさした')).toBe(true);
    expect(juExpanded.physicalForms.some(form => form.surface === '唱さした')).toBe(false);
  });

  test('reviewed target-order compatibility preserves the qualified narrow 逸さす target', () => {
    const competing = pairedPhysicalPrimary(1587490, [
      { route: 'kanji', surface: '逸さす', counterpart: 'いっさす' },
      { route: 'kanji', surface: '佚さす', counterpart: 'いっさす' },
      { route: 'kana', surface: 'いっさす', counterpart: '逸さす' }
    ]);
    const qualified = pairedPhysicalPrimary(2410170, [
      { route: 'kanji', surface: '逸さす', counterpart: 'いっさす' },
      { route: 'kana', surface: 'いっさす', counterpart: '逸さす' }
    ]);
    const allocator = new StreamingPhysicalTargetAllocator([], 10_000_000, [{
      id: 'qualified-order-witness',
      kind: 'physical-target-order',
      seq: qualified.rootSeq,
      competingCreatorSeq: competing.rootSeq,
      property: qualified.first,
      provenance: { source: 'test' },
      preservedBehavior: 'Keep the qualified narrow target.'
    }]);
    const wide = allocator.add({
      ordinal: 0,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 0,
      emission: competing
    });
    const narrow = allocator.add({
      ordinal: 1,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 1,
      emission: qualified
    });
    expect(narrow.targetSeq).not.toBe(wide.targetSeq);
    expect(allocator.finish()).toHaveLength(2);
    expect(allocator.target(narrow.targetSeq)).toEqual(expect.objectContaining({
      kanji: ['逸さす'],
      kana: ['いっさす']
    }));
  });

  test('rejects stale physical-target ordering compatibility', () => {
    const emission = physicalPrimary(2_410_170, 'kana', 'いっさす');
    const allocator = new StreamingPhysicalTargetAllocator([], 10_000_000, [{
      id: 'stale-order-witness',
      kind: 'physical-target-order',
      seq: emission.rootSeq,
      competingCreatorSeq: 1_587_490,
      property: emission.first,
      provenance: { source: 'test' },
      preservedBehavior: 'This deliberately stale row must be rejected.'
    }]);
    allocator.add({
      ordinal: 0,
      firstAlias: 1,
      secondAlias: null,
      creationPrecedence: 0,
      emission
    });
    expect(() => allocator.finish()).toThrow('stale-order-witness is stale');
  });

  test('reviewed same-root order preserves the qualified narrow 居られます target', () => {
    const withProperty = (
      emission: ConjugationEmission,
      type: number
    ): ConjugationEmission => {
      const rule = {
        ...emission.first,
        pos: 'v1',
        type,
        negative: false,
        formal: true
      };
      const physicalForms = emission.physicalForms.map(form => ({
        ...form,
        firstRule: { ...form.firstRule, ...rule }
      }));
      return { ...emission, first: rule, physicalForms, forms: physicalForms };
    };
    const broad = withProperty(pairedPhysicalPrimary(1577980, [
      { route: 'kana', surface: 'いられます', counterpart: null },
      { route: 'kana', surface: 'いれます', counterpart: null }
    ]), 5);
    const narrow = withProperty(pairedPhysicalPrimary(1577980, [
      { route: 'kana', surface: 'いられます', counterpart: null }
    ]), 6);
    const later = withProperty(pairedPhysicalPrimary(2809790, [
      { route: 'kana', surface: 'いられます', counterpart: null }
    ]), 1);
    const compatibility = [{
      id: 'qualified-same-root-order-witness',
      kind: 'physical-target-order' as const,
      seq: narrow.rootSeq,
      competingCreatorSeq: broad.rootSeq,
      property: narrow.first,
      provenance: { source: 'test' },
      preservedBehavior: 'Keep the qualified narrow type-6 target.'
    }];
    const allocate = (rows: typeof compatibility) => {
      const allocator = new StreamingPhysicalTargetAllocator([], 10_000_000, rows);
      const wide = allocator.add({
        ordinal: 0,
        firstAlias: 1,
        secondAlias: null,
        creationPrecedence: 0,
        emission: broad
      });
      const selected = allocator.add({
        ordinal: 1,
        firstAlias: 2,
        secondAlias: null,
        creationPrecedence: 1,
        emission: narrow
      });
      const reused = allocator.add({
        ordinal: 2,
        firstAlias: 3,
        secondAlias: null,
        creationPrecedence: 2,
        emission: later
      });
      return { allocator, wide, selected, reused };
    };
    expect(allocate([]).selected.targetSeq).toBe(10_000_000);
    const qualified = allocate(compatibility);
    expect(qualified.selected.targetSeq).not.toBe(qualified.wide.targetSeq);
    expect(qualified.reused.targetSeq).toBe(qualified.selected.targetSeq);
    expect(qualified.allocator.target(qualified.selected.targetSeq).kana)
      .toEqual(['いられます']);
  });

  test('2089020 だ reuses the lexical です target', () => {
    const da = entry(2089020, [], ['だ', 'じゃ'], 'cop-da', new Set(['じゃ']));
    const formal = property(emitPrimaryConjugations(da), 1, false, true);
    expect(surfaces(formal)).toEqual(['です']);
    expect(formal.first.pos).toBe('cop');

    const lexicalDesu: PhysicalTarget = {
      seq: 1628500,
      kanji: [],
      kana: ['です'],
      secondaryForms: [],
      conjugatable: false,
      origin: 'lexical'
    };
    const physical = assignPhysicalTargets([formal], [lexicalDesu], 10_000_000);
    expect(physical.bindings[0]?.targetSeq).toBe(1628500);
    expect(physical.targets).toEqual([lexicalDesu]);
  });

  test('1253900 欠く does not inherit the physical target member 缺け', () => {
    const kaku = entry(1253900, ['欠く', '闕く'], ['かく'], 'v5k');
    const potential = property(
      emitPrimaryConjugations(kaku, { types: new Set([5]) }),
      5,
      false,
      false
    );
    expect(surfaces(potential)).toEqual(['欠ける', '闕ける', 'かける']);

    const lexicalPotential: PhysicalTarget = {
      seq: 1253920,
      kanji: ['欠ける', '缺ける', '闕ける'],
      kana: ['かける'],
      secondaryForms: [
        { route: 'kanji', text: '欠ける', counterpart: 'かける' },
        { route: 'kanji', text: '缺ける', counterpart: 'かける' },
        { route: 'kanji', text: '闕ける', counterpart: 'かける' },
        { route: 'kana', text: 'かける', counterpart: '欠ける' }
      ],
      conjugatable: true,
      origin: 'lexical'
    };
    const secondary = emitSecondaryConjugations(potential, { types: new Set([2]) });
    const past = property(secondary, 2, false, false);
    expect(surfaces(past)).toEqual(['欠けた', '闕けた', 'かけた']);
    expect(surfaces(past)).not.toContain('缺けた');

    const physical = assignPhysicalTargets([potential, past], [lexicalPotential], 10_000_000);
    const potentialBinding = physical.bindings.find(binding =>
      binding.emissionKey === conjugationEmissionKey(potential));
    const pastBinding = physical.bindings.find(binding =>
      binding.emissionKey === conjugationEmissionKey(past));
    expect(potentialBinding?.targetSeq).toBe(1253920);
    expect(pastBinding?.viaTargetSeq).toBe(1253920);
  });

  test('the five witnesses have a stable complete-key digest', () => {
    const witnesses = [
      entry(1519210, ['忘れる'], ['わすれる'], 'v1'),
      entry(1337800, ['熟す'], ['じゅくす'], 'v5s'),
      entry(1358280, ['食べる', '喰べる'], ['たべる'], 'v1'),
      entry(2089020, [], ['だ', 'じゃ'], 'cop-da', new Set(['じゃ'])),
      entry(1253900, ['欠く', '闕く'], ['かく'], 'v5k')
    ];
    const summary = summarizeConjugationRelation(witnesses.flatMap(emitCanonicalConjugations));
    expect(summary).toEqual({
      emissions: 809,
      surfaces: 2465,
      uniqueKeys: 2465,
      duplicates: 0,
      sha256: '75e98fec671acd3e46a806ea8d68c6a254d4444d5fb6e0eb42554b2db915f924'
    });
  });
});
