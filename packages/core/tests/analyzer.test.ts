import { describe, expect, test } from 'bun:test';
import { fileURLToPath } from 'node:url';
import { gunzipSync } from 'node:zlib';
import { buildAnalyzerSupport, type AnalyzerSupportSource } from '../../data/src/browser-pack/analyzer-support.js';
import { buildDetailStore } from '../../data/src/browser-pack/details.js';
import { encodeMorphologyArtifact, type CompiledMorphologyArtifact } from '../../data/src/browser-pack/morphology-format.js';
import { buildRootPayload, type RootPayloadSource } from '../../data/src/browser-pack/root-payload.js';
import {
  type PortableLegacyCompactResult,
  type PortableLegacyGlossJson,
  type PortableLegacyTransformedResult
} from '../src/legacy-contract.js';
import {
  memoryDetailSource,
  openAnalyzerSupport,
  openDetailStore,
  openMorphology,
  openRootPayload,
  SurfaceIndex
} from '../src/compiler.js';
import {
  openPortableAnalyzer,
  serializePortableLegacyDetailed,
  type PortableAnalyzerAnnotations
} from '../src/qualification.js';

const encoder = new TextEncoder();
const decoder = new TextDecoder();
const surfaceCompiler = fileURLToPath(
  new URL('../../data/tools/surface-index/Cargo.toml', import.meta.url)
);

function utf8Compare(left: string, right: string): number {
  const a = encoder.encode(left);
  const b = encoder.encode(right);
  for (let index = 0; index < Math.min(a.length, b.length); index++) {
    if (a[index] !== b[index]) return a[index]! - b[index]!;
  }
  return a.length - b.length;
}

function surfaceBytes(
  extraDirect: readonly string[] = [],
  extraMorphology: readonly string[] = []
): Uint8Array {
  const direct = [
    'する', 'たい', 'たべる', 'ねこ', 'はし', '他', '何', '例', '本', '猫', '食べる',
    ...extraDirect
  ];
  const morphology = [
    'たべた', 'たべ', 'たべない', '食べた', '食べ', '食べたた', '食べない',
    ...extraMorphology
  ];
  const values = [...new Set([...direct, ...morphology])].sort(utf8Compare);
  const input = values.map(surface => {
    const kana = /^[ァ-ヺヽヾーぁ-ゔゝゞ]+$/u.test(surface);
    return [
      surface,
      Number(kana && direct.includes(surface)),
      Number(kana && morphology.includes(surface)),
      Number(!kana && direct.includes(surface)),
      Number(!kana && morphology.includes(surface))
    ].join('\t');
  }).join('\n') + '\n';
  const result = Bun.spawnSync([
    'cargo', 'run', '--quiet', '--release', '--manifest-path', surfaceCompiler
  ], { stdin: encoder.encode(input), stdout: 'pipe', stderr: 'pipe' });
  if (result.exitCode !== 0) throw new Error(decoder.decode(result.stderr));
  return result.stdout;
}

const rootSource: RootPayloadSource = {
  entries: [
    { seq: 100, nKanji: 1, nKana: 1, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] },
    { seq: 200, nKanji: 1, nKana: 1, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['v1'] },
    { seq: 201, nKanji: 1, nKana: 1, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['v1'] },
    { seq: 300, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['aux-adj'] },
    { seq: 400, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['vs-i'] },
    { seq: 500, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] },
    { seq: 600, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['ctr'] },
    { seq: 700, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] },
    { seq: 701, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] },
    { seq: 800, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['pn'] },
    { seq: 801, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] },
    { seq: 802, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] },
    { seq: 803, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
      preferKana: false, preferKanaOnOrdinalZero: false, pos: ['pn'] }
  ],
  forms: [
    { surface: '猫', route: 'kanji', seq: 100, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: false, best: 'ねこ' },
    { surface: 'ねこ', route: 'kana', seq: 100, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: false, best: '猫' },
    { surface: '食べる', route: 'kanji', seq: 200, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: true, nokanji: false, best: 'たべる' },
    { surface: 'たべる', route: 'kana', seq: 200, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: true, nokanji: false, best: '食べる' },
    { surface: '食べる', route: 'kanji', seq: 201, ord: 0, common: null, commonTags: '',
      conjugatable: true, nokanji: false, best: 'たべる' },
    { surface: 'たべる', route: 'kana', seq: 201, ord: 0, common: null, commonTags: '',
      conjugatable: true, nokanji: false, best: '食べる' },
    { surface: 'たい', route: 'kana', seq: 300, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: true, best: null },
    { surface: 'する', route: 'kana', seq: 400, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: true, nokanji: true, best: null },
    { surface: '例', route: 'kanji', seq: 500, ord: 0, common: null, commonTags: '',
      conjugatable: false, nokanji: false, best: null },
    { surface: '本', route: 'kanji', seq: 600, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: false, best: null },
    { surface: 'はし', route: 'kana', seq: 700, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: true, best: null },
    { surface: 'はし', route: 'kana', seq: 701, ord: 1, common: null, commonTags: '',
      conjugatable: false, nokanji: true, best: null },
    { surface: '何', route: 'kanji', seq: 800, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: false, best: 'なに' },
    { surface: '他', route: 'kanji', seq: 801, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: false, best: 'た' },
    { surface: '他', route: 'kanji', seq: 802, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: false, best: 'ほか' },
    { surface: '何', route: 'kanji', seq: 803, ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: false, nokanji: false, best: 'なん' }
  ],
  restrictions: []
};

const morphologySource: CompiledMorphologyArtifact = {
  positions: ['v1'],
  rules: [
    { pos: 'v1', type: 1, negative: true, formal: false, ordinal: 2,
      stem: 1, okuri: 'ない', euphr: '', euphk: '' },
    { pos: 'v1', type: 13, negative: false, formal: false, ordinal: 0,
      stem: 1, okuri: '', euphr: '', euphk: '' },
    { pos: 'v1', type: 2, negative: false, formal: false, ordinal: 1,
      stem: 1, okuri: 'た', euphr: '', euphk: '' }
  ],
  templates: [
    { suffix: '', removed: 'る', firstRule: 1, secondRule: null },
    { suffix: 'た', removed: 'る', firstRule: 2, secondRule: null },
    { suffix: 'ない', removed: 'る', firstRule: 0, secondRule: null }
  ],
  rootKeys: [
    { route: 'kana', pos: 'v1', sourceText: 'たべる', records: [
      { rootGroup: 0, sourceForm: 'たべる', sourceReading: 'たべる', ord: 0, common: 1 },
      { rootGroup: 1, sourceForm: 'たべる', sourceReading: 'たべる', ord: 0, common: null }
    ] },
    { route: 'kanji', pos: 'v1', sourceText: '食べる', records: [
      { rootGroup: 0, sourceForm: '食べる', sourceReading: 'たべる', ord: 0, common: 1 },
      { rootGroup: 1, sourceForm: '食べる', sourceReading: 'たべる', ord: 0, common: null }
    ] }
  ],
  rootGroups: [
    { seq: 200, forms: ['たべる', '食べる'] },
    { seq: 201, forms: ['たべる', '食べる'] }
  ],
  patches: [{
    route: 'kanji', surface: '食べたた', rootSeq: 200,
    sourceText: '食べる', sourceForm: '食べる', sourceReading: 'たべる',
    form: '食べたた', reading: 'たべたた', firstRule: 1, secondRule: 2,
    intermediate: '食べ', ord: 0, common: 1
  }],
  tombstones: []
};

const supportSource: AnalyzerSupportSource = {
  suffixes: [
    { text: 'たい', values: [{ keyword: ':tai', form: {
      seq: 300, text: 'たい', bestKanji: null, commonTags: 'ichi1', ord: 0,
      common: 1, conjugatable: false, nokanji: true, conjugations: ':root'
    } }] },
    { text: 'たべた', values: [{ keyword: ':tai', form: {
      seq: 900, text: 'たべた', bestKanji: null, commonTags: '', ord: 0,
      common: null, conjugatable: true, nokanji: true,
      conjugations: [{
        seq: 900, from: 200, via: null, pos: 'v1', type: 2,
        negative: false, formal: false
      }]
    } }] },
    { text: 'ず', values: [{ keyword: ':nai-n', form: null }] }
  ],
  suffixClasses: [{ seq: 300, keyword: ':tai' }, { seq: 900, keyword: ':tai' }],
  counters: [{
    key: '本', order: 0, className: 'CounterText', text: '本', kana: 'ほん', suffix: null,
    source: { seq: 600, route: 'kanji', text: '本', ord: 0 }, ordinal: false,
    foreign: false, common: 1, suffixDescriptions: [], digitOptions: [], digitSet: [], allowed: []
  }],
  splits: [{
    definitionSeq: 500, route: 'kanji', surface: '例', kind: 'split',
    parts: [':score'], score: 20, primary: 0, connector: '', root: []
  }, {
    definitionSeq: 100, route: 'kanji', surface: '猫', kind: 'split',
    parts: [], score: 20, primary: 0, connector: '', root: []
  }],
  hints: [],
  collisions: [200, 201].map(rootSeq => ({
    rootSeq, collisionSeq: 900, viaSeq: null, route: 'kana' as const,
    surface: 'たべた', ruleIds: [2] as const,
    nKanji: 0, nKana: 1, primaryNokanji: false, archived: false,
    preferKana: false, preferKanaOnOrdinalZero: false, pos: ['v1'],
    skipWord: false, finalParticle: false, semiFinalParticle: false,
    nonFinalParticle: false, copula: false, noKanjiBreakPenalty: false
  })),
  generated: {
    ruleAliases: [0, 1, 2], aliasCount: 3, records: [], semanticPaths: 0,
    matchedPaths: 0, countExceptions: 0, physicalGroups: 0, physicalMembers: 0,
    propertyOverrides: 0, maxMemberOrd: 0, maxViaMemberOrd: 0, maxPropOrd: 0,
    projectionSha256: ''
  }
};

function analyzer(
  generated?: NonNullable<PortableAnalyzerAnnotations['generated']>,
  lookupOrder?: NonNullable<PortableAnalyzerAnnotations['lookupOrder']>,
  morphology: CompiledMorphologyArtifact = morphologySource,
  roots: RootPayloadSource = rootSource,
  surfaces: Uint8Array = surfaceBytes(),
  supportInput: AnalyzerSupportSource = supportSource
) {
  const support = openAnalyzerSupport(buildAnalyzerSupport(supportInput).bytes);
  return openPortableAnalyzer({
    surface: new SurfaceIndex(surfaces),
    roots: openRootPayload(buildRootPayload(roots).bytes),
    morphology: openMorphology(encodeMorphologyArtifact(morphology)),
    support,
    annotations: generated || lookupOrder ? {
      split: (...args) => support.split(...args),
      hint: (...args) => support.hint(...args),
      generated,
      lookupOrder
    } : undefined
  });
}

async function detailReader(source: RootPayloadSource = rootSource) {
  const bytes = buildDetailStore(source.entries.map(entry => ({
    seq: entry.seq,
    forms: source.forms
      .filter(form => form.seq === entry.seq)
      .map(({ surface, ...form }) => ({ ...form, text: surface })),
    senses: []
  })), { targetBlockBytes: 1024 }).bytes;
  return openDetailStore(
    memoryDetailSource(bytes),
    async compressed => new Uint8Array(gunzipSync(compressed))
  );
}

function firstDetailedWord(
  detailed: PortableLegacyTransformedResult
): PortableLegacyGlossJson {
  const chunk = detailed[0];
  if (typeof chunk === 'string' || !chunk) throw new Error('Expected a word chunk');
  return chunk[0]![0][0]![1];
}

describe('portable analyzer end to end', () => {
  test('materializes direct roots, reverse morphology, suffixes, splits, and counters', () => {
    const runtime = analyzer();
    expect(runtime.analyze('猫', { limit: 1 }).paths[0]?.tokens[0]).toMatchObject({
      text: '猫', reading: 'ねこ', root: { seq: 100, form: '猫', reading: 'ねこ' }
    });
    expect(runtime.analyze('食べた', { limit: 1 }).paths[0]?.tokens[0]).toMatchObject({
      text: '食べた', reading: 'たべた', root: { seq: 200 },
      inflection: [{ pos: 'v1', type: 2, negative: false, formal: false, ordinal: 1 }]
    });
    expect(runtime.analyze('食べたい', { limit: 1 }).paths[0]?.tokens[0]).toMatchObject({
      text: '食べたい', root: { seq: 200 }, components: [{ text: '食べ' }, { text: 'たい' }]
    });
    expect(runtime.analyze('食べず', { limit: 1 }).paths[0]?.tokens).toMatchObject([{
      text: '食べず', root: { seq: 200 },
      inflection: [{ pos: 'v1', type: 1, negative: true }]
    }]);
    const split = runtime.analyze('例', { limit: 1 }).paths[0]?.tokens[0];
    expect(split?.score).toBeGreaterThan(20);
    expect(runtime.analyze('3本', { limit: 1 }).paths[0]?.tokens[0]).toMatchObject({
      text: '3本', reading: 'さんぽん', root: { seq: 600 }, counter: ['Value: 3', false]
    });
  });

  test('preserves top-N alternatives, entity hints, chunk boundaries, and legacy identity', async () => {
    const runtime = analyzer();
    const alternatives = runtime.analyze('はし', { limit: 2 }).paths[0]?.tokens[0];
    expect(alternatives?.alternatives.length).toBeGreaterThan(0);

    const naniAnalysis = runtime.analyze('何他', { limit: 3 });
    const nani = naniAnalysis.paths[0]?.tokens;
    expect(nani?.map(token => token.text)).toEqual(['何', '他']);
    expect(nani?.[1]?.alternatives.map(value => value.reading).sort()).toEqual(['た', 'ほか']);
    expect(nani?.[0]?.reading).toBe('なに');
    const naniCompact = runtime.serializeLegacy(naniAnalysis) as PortableLegacyCompactResult;
    const naniCompactToken = naniCompact[0];
    if (typeof naniCompactToken === 'string') throw new Error('Expected a word chunk');
    expect(naniCompactToken[0]![0][0]![0]).toBe('nani');
    expect(naniCompactToken[0]![0][0]![1].kana).toBe('なに');
    expect(naniCompactToken[0]![0][0]![1].components).toHaveLength(2);
    const naniDetailed = await runtime.serializeLegacyDetailed(
      naniAnalysis,
      await detailReader()
    );
    const naniDetailedChunk = naniDetailed[0];
    if (typeof naniDetailedChunk === 'string' || !naniDetailedChunk) {
      throw new Error('Expected a detailed word chunk');
    }
    expect(naniDetailedChunk[0]![0][0]![0]).toBe('nani');
    expect(firstDetailedWord(naniDetailed).alternative).toHaveLength(2);

    const entity = runtime.analyze('未知', {
      limit: 1,
      entities: [{ start: 0, end: 2, boost: 100 }]
    }).paths[0]?.tokens[0];
    expect(entity).toMatchObject({ text: '未知', entity: true, root: null });

    const catScore = runtime.analyze('猫', { limit: 1 }).paths[0]!.score;
    expect(catScore).toBeGreaterThan(20);
    const exampleScore = runtime.analyze('例', { limit: 1 }).paths[0]!.score;
    const punctuation = runtime.analyze('猫。例', { limit: 1 });
    expect(punctuation.chunks.map(chunk => [chunk.type, chunk.text])).toEqual([
      ['word', '猫'], ['misc', '。'], ['word', '例']
    ]);
    expect(punctuation.paths[0]?.score).toBe(catScore + exampleScore);
    expect(punctuation.paths[0]?.tokens.map(token => [token.text, token.route])).toEqual([
      ['猫', 'kanji'], ['。', 'gap'], ['例', 'kanji']
    ]);

    const normalizedPunctuation = runtime.analyze('猫。', {
      limit: 1,
      normalizePunctuation: true
    });
    expect(normalizedPunctuation.normalized).toBe('猫. ');
    expect(normalizedPunctuation.paths[0]?.tokens.map(token => [token.text, token.route])).toEqual([
      ['猫', 'kanji'], ['. ', 'gap']
    ]);

    const result = runtime.analyze('食べた', { limit: 1 });
    expect(JSON.stringify(runtime.serializeLegacy(result))).toContain('"seq":200');
    const nested = runtime.serializeLegacy(punctuation) as unknown[];
    expect(nested).toHaveLength(3);
    expect(nested[1]).toBe('。');
    expect(runtime.romanize('猫')).toBe('neko');

    const gapAnalysis = runtime.analyze('猫を猫', { limit: 1 });
    expect(gapAnalysis.chunks.map(chunk => [chunk.type, chunk.text])).toEqual([
      ['word', '猫を猫']
    ]);
    const compact = runtime.serializeLegacy(gapAnalysis) as readonly (readonly [
      readonly (readonly [string, unknown, unknown])[],
      number
    ][])[];
    expect(compact[0]![0]![0].map(value => value[0])).toEqual(['neko', 'wo', 'neko']);
    expect(runtime.romanize('猫を猫')).toBe('neko wo neko');
    expect(runtime.romanize('猫。猫')).toBe('neko。 neko');
  });

  test('keeps only explicitly selected semantic suffix conjugations', async () => {
    const runtime = analyzer();
    const analysis = runtime.analyze('食べたべた', { limit: 1 });
    const token = analysis.paths[0]?.tokens[0];
    expect(token).toMatchObject({
      text: '食べたべた',
      components: [
        { text: '食べ', root: { seq: 200 } },
        {
          text: 'たべた',
          root: { seq: 200 },
          inflection: [{ pos: 'v1', type: 2 }]
        }
      ]
    });

    const detailed = await runtime.serializeLegacyDetailed(analysis, await detailReader());
    const word = firstDetailedWord(detailed);
    const suffix = word.components?.[1];
    // Legacy seq names the complete physical target even when the suffix DSL
    // explicitly selects one conjugation row for presentation.
    expect(suffix?.seq).toEqual([200, 201]);
    expect(suffix?.conj).toEqual([expect.objectContaining({
      reading: 'たべる'
    })]);
  });

  test('finds an explicit suffix member beyond an earlier same-root property', async () => {
    const shadowMorphology: CompiledMorphologyArtifact = {
      ...morphologySource,
      rules: [...morphologySource.rules, {
        pos: 'v1', type: 5, negative: false, formal: false, ordinal: 3,
        stem: 1, okuri: 'た', euphr: '', euphk: ''
      }],
      templates: [
        morphologySource.templates[0]!,
        morphologySource.templates[1]!,
        { suffix: 'た', removed: 'る', firstRule: 3, secondRule: null },
        morphologySource.templates[2]!
      ]
    };
    const shadowSupport: AnalyzerSupportSource = {
      ...supportSource,
      generated: {
        ...supportSource.generated!,
        ruleAliases: [0, 1, 2, 3],
        aliasCount: 4
      }
    };
    const runtime = analyzer(
      undefined,
      (_route, _surface, rootSeq, aliases) =>
        rootSeq === 900 && aliases === null ? 1 : aliases?.[0] === 3 ? 0 : 1,
      shadowMorphology,
      rootSource,
      surfaceBytes(),
      shadowSupport
    );

    const analysis = runtime.analyze('食べたべた', { limit: 1 });
    const suffix = analysis.paths[0]?.tokens[0]?.components[1];
    expect(suffix).toMatchObject({
      text: 'たべた', root: { seq: 200 },
      inflection: [{ pos: 'v1', type: 2, negative: false, formal: false }]
    });
    const detailed = await runtime.serializeLegacyDetailed(analysis, await detailReader());
    expect(firstDetailedWord(detailed).components?.[1]?.conj).toEqual([
      expect.objectContaining({
        prop: [{ pos: 'v1', type: 'Past (~ta)' }],
        reading: 'たべる'
      })
    ]);

    const missingSupport: AnalyzerSupportSource = {
      ...shadowSupport,
      suffixes: shadowSupport.suffixes.map(value => value.text === 'たべた' ? {
        text: 'たべた',
        values: [{ keyword: ':tai', form: {
          seq: 900, text: 'たべた', bestKanji: null, commonTags: '', ord: 0,
          common: null, conjugatable: true, nokanji: true,
          conjugations: [{
            seq: 900, from: 200, via: null, pos: 'v1', type: 99,
            negative: false, formal: false
          }]
        } }]
      } : value)
    };
    const missing = analyzer(
      undefined,
      (_route, _surface, rootSeq, aliases) =>
        rootSeq === 900 && aliases === null ? 1 : aliases?.[0] === 3 ? 0 : 1,
      shadowMorphology,
      rootSource,
      surfaceBytes(),
      missingSupport
    );
    expect(() => missing.analyze('食べたべた', { limit: 1 }))
      .toThrow('Explicit suffix member is unavailable for "たべた"');
  });

  test('keeps a lexical suffix target public while retaining its selected semantic conjugation', async () => {
    const lexicalTargetRoots: RootPayloadSource = {
      entries: [...rootSource.entries, {
        seq: 900, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
        preferKana: false, preferKanaOnOrdinalZero: false, pos: ['v1']
      }],
      forms: [...rootSource.forms, {
        surface: 'たべた', route: 'kana', seq: 900, ord: 0, common: null,
        commonTags: '', conjugatable: true, nokanji: true, best: null
      }],
      restrictions: rootSource.restrictions
    };
    const runtime = analyzer(
      undefined,
      undefined,
      morphologySource,
      lexicalTargetRoots,
      surfaceBytes(['たべた'])
    );
    const analysis = runtime.analyze('食べたべた', { limit: 1 });
    const suffix = analysis.paths[0]?.tokens[0]?.components[1];
    expect(suffix).toMatchObject({
      text: 'たべた', root: { seq: 900 }, inflection: []
    });

    const detailed = await runtime.serializeLegacyDetailed(
      analysis,
      await detailReader(lexicalTargetRoots)
    );
    const detailedSuffix = firstDetailedWord(detailed).components?.[1];
    expect(detailedSuffix?.seq).toBe(900);
    expect(detailedSuffix?.conj).toEqual([expect.objectContaining({
      reading: 'たべる'
    })]);
  });

  test('restores conditional abbreviations with kana before recursive lookup', () => {
    const roots: RootPayloadSource = {
      entries: [...rootSource.entries, {
        seq: 804, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
        preferKana: false, preferKanaOnOrdinalZero: false, pos: ['v1']
      }],
      forms: [...rootSource.forms, {
        surface: 'たべれば', route: 'kana', seq: 804, ord: 0, common: 1,
        commonTags: 'ichi1', conjugatable: true, nokanji: true, best: null
      }],
      restrictions: rootSource.restrictions
    };
    const support: AnalyzerSupportSource = {
      ...supportSource,
      suffixes: [...supportSource.suffixes, {
        text: 'りゃ', values: [{ keyword: ':reba', form: null }]
      }]
    };
    const runtime = analyzer(
      undefined,
      undefined,
      morphologySource,
      roots,
      surfaceBytes(['たべれば']),
      support
    );

    expect(runtime.analyze('たべりゃ', { limit: 1 }).paths[0]?.tokens).toMatchObject([{
      text: 'たべりゃ',
      reading: 'たべりゃ',
      root: { seq: 804 }
    }]);
  });

  test('keeps semantic split scoring when morphology resolves to a lexical collision', () => {
    const support: AnalyzerSupportSource = {
      ...supportSource,
      splits: [...supportSource.splits, {
        definitionSeq: 200,
        route: 'kana',
        surface: 'たべた',
        kind: 'split',
        parts: [':score'],
        score: 20,
        primary: 0,
        connector: '',
        root: []
      }]
    };
    const baseline = analyzer().analyze('たべた', { limit: 1 }).paths[0]?.tokens[0];
    const split = analyzer(
      undefined,
      undefined,
      morphologySource,
      rootSource,
      surfaceBytes(),
      support
    ).analyze('たべた', { limit: 1 }).paths[0]?.tokens[0];

    expect(split).toMatchObject({ text: 'たべた', root: { seq: 200 } });
    expect(split!.score).toBeGreaterThan(baseline!.score);
  });

  test('prefers a physical collision split over its semantic ancestor split', () => {
    const support: AnalyzerSupportSource = {
      ...supportSource,
      splits: [...supportSource.splits, {
        definitionSeq: 200,
        route: 'kana',
        surface: 'たべた',
        kind: 'split',
        parts: [':score'],
        score: 20,
        primary: 0,
        connector: '',
        root: []
      }, {
        definitionSeq: 900,
        route: 'kana',
        surface: 'たべた',
        kind: 'split',
        parts: [':score'],
        score: 5,
        primary: 0,
        connector: '',
        root: []
      }]
    };
    const baseline = analyzer().analyze('たべた', { limit: 1 }).paths[0]?.tokens[0];
    const split = analyzer(
      undefined,
      undefined,
      morphologySource,
      rootSource,
      surfaceBytes(),
      support
    ).analyze('たべた', { limit: 1 }).paths[0]?.tokens[0];

    expect(split).toMatchObject({ text: 'たべた', root: { seq: 200 } });
    expect(split!.score).toBe(baseline!.score + 5);
  });

  test('lazily applies the suru suffix cap to generated v5s words at kanji breaks', () => {
    const roots: RootPayloadSource = {
      entries: [...rootSource.entries,
        { seq: 804, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] },
        { seq: 805, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['v5s'] },
        { seq: 806, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['aux-v'] },
        { seq: 807, nKanji: 2, nKana: 0, primaryNokanji: false, archived: true,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] }
      ],
      forms: [...rootSource.forms,
        { surface: '邪', route: 'kanji', seq: 804, ord: 0, common: 1,
          commonTags: 'ichi1', conjugatable: false, nokanji: false, best: null },
        { surface: '推す', route: 'kanji', seq: 805, ord: 0, common: 1,
          commonTags: 'ichi1', conjugatable: true, nokanji: false, best: 'おす' },
        { surface: 'した', route: 'kana', seq: 806, ord: 0, common: 1,
          commonTags: 'ichi1', conjugatable: false, nokanji: true, best: null },
        { surface: '邪推した', route: 'kanji', seq: 807, ord: 0, common: null,
          commonTags: '', conjugatable: false, nokanji: false, best: null }
      ],
      restrictions: rootSource.restrictions
    };
    const morphology: CompiledMorphologyArtifact = {
      ...morphologySource,
      positions: [...morphologySource.positions, 'v5s'],
      rules: [...morphologySource.rules, {
        pos: 'v5s', type: 2, negative: false, formal: false, ordinal: 0,
        stem: 1, okuri: 'した', euphr: '', euphk: ''
      }],
      templates: [
        morphologySource.templates[0]!,
        { suffix: 'した', removed: 'す', firstRule: 3, secondRule: null },
        ...morphologySource.templates.slice(1)
      ],
      rootKeys: [...morphologySource.rootKeys, {
        route: 'kanji', pos: 'v5s', sourceText: '推す', records: [{
          rootGroup: 2, sourceForm: '推す', sourceReading: 'おす', ord: 0, common: 1
        }]
      }],
      rootGroups: [...morphologySource.rootGroups, { seq: 805, forms: ['推す'] }]
    };
    const supportBase: AnalyzerSupportSource = {
      ...supportSource,
      generated: {
        ...supportSource.generated,
        ruleAliases: [0, 1, 2, 3],
        aliasCount: 4
      }
    };
    const support: AnalyzerSupportSource = {
      ...supportBase,
      suffixes: [...supportSource.suffixes, {
        text: 'した', values: [{ keyword: ':suru', form: {
          seq: 806, text: 'した', bestKanji: null, commonTags: 'ichi1', ord: 0,
          common: 1, conjugatable: false, nokanji: true, conjugations: ':root'
        } }]
      }],
      suffixClasses: [...supportSource.suffixClasses, { seq: 806, keyword: ':suru' }]
    };
    const surfaces = surfaceBytes(
      ['邪', '推す', 'した', '邪推した'],
      ['推した']
    );
    const tokenScore = (supportInput: AnalyzerSupportSource): number => {
      const result = analyzer(
        undefined, undefined, morphology, roots, surfaces, supportInput
      ).analyze('邪推した', { limit: 10 });
      const path = result.paths.find(value =>
        value.tokens.map(token => token.text).join('|') === '邪|推した');
      const token = path?.tokens[1];
      if (!token) throw new Error('Expected generated v5s split path');
      return token.score;
    };

    expect(tokenScore(support)).toBeLessThan(tokenScore(supportBase));
  });

  test('scores an explicitly selected lexical target as conjugation-only', () => {
    const roots: RootPayloadSource = {
      entries: [...rootSource.entries,
        { seq: 804, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n'] },
        { seq: 805, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['aux-adj'] }
      ],
      forms: [...rootSource.forms,
        { surface: '食べ', route: 'kanji', seq: 804, ord: 0, common: 3,
          commonTags: 'news1', conjugatable: false, nokanji: false, best: null },
        { surface: 'そう', route: 'kana', seq: 805, ord: 0, common: 1,
          commonTags: 'ichi1', conjugatable: false, nokanji: true, best: null }
      ],
      restrictions: rootSource.restrictions
    };
    const support: AnalyzerSupportSource = {
      ...supportSource,
      suffixes: [...supportSource.suffixes, {
        text: 'そう', values: [{ keyword: ':sou', form: {
          seq: 805, text: 'そう', bestKanji: null, commonTags: 'ichi1', ord: 0,
          common: 1, conjugatable: false, nokanji: true, conjugations: ':root'
        } }]
      }],
      suffixClasses: [...supportSource.suffixClasses, { seq: 805, keyword: ':sou' }],
      collisions: [...supportSource.collisions, {
        rootSeq: 200, collisionSeq: 804, viaSeq: null, route: 'kanji',
        surface: '食べ', ruleIds: [1], nKanji: 1, nKana: 0,
        primaryNokanji: false, archived: false, preferKana: false,
        preferKanaOnOrdinalZero: false, pos: ['n', 'v1'], skipWord: false,
        finalParticle: false, semiFinalParticle: false, nonFinalParticle: false,
        copula: false, noKanjiBreakPenalty: false
      }]
    };
    const runtime = analyzer(
      undefined,
      undefined,
      morphologySource,
      roots,
      surfaceBytes(['食べ', 'そう']),
      support
    );
    const analysis = runtime.analyze('食べそう', { limit: 2 });

    expect(analysis.paths[0]?.tokens).toMatchObject([{
      text: '食べそう',
      score: 308,
      components: [
        { text: '食べ', root: { seq: 804 } },
        { text: 'そう', root: { seq: 805 } }
      ]
    }]);
  });

  test('keeps archived prefix closure when a suffix selects physical members', () => {
    const roots: RootPayloadSource = {
      entries: [
        ...rootSource.entries,
        { seq: 900, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['v1'] },
        { seq: 901, nKanji: 1, nKana: 0, primaryNokanji: false, archived: true,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['v5b'] },
        { seq: 902, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
          preferKana: false, preferKanaOnOrdinalZero: false, pos: ['aux-adj'] }
      ],
      forms: [
        ...rootSource.forms,
        { surface: '食べる', route: 'kanji', seq: 900, ord: 0, common: 0,
          commonTags: 'ichi1', conjugatable: true, nokanji: false, best: null },
        { surface: '食ぶ', route: 'kanji', seq: 901, ord: 0, common: null,
          commonTags: '', conjugatable: true, nokanji: false, best: null },
        { surface: 'そう', route: 'kana', seq: 902, ord: 0, common: 0,
          commonTags: 'ichi1', conjugatable: false, nokanji: true, best: null }
      ],
      restrictions: []
    };
    const morphology: CompiledMorphologyArtifact = {
      positions: ['v1', 'v5b'],
      rules: [
        { pos: 'v1', type: 13, negative: false, formal: false, ordinal: 0,
          stem: 1, okuri: '', euphr: '', euphk: '' },
        { pos: 'v5b', type: 5, negative: false, formal: false, ordinal: 0,
          stem: 1, okuri: 'べる', euphr: '', euphk: '' }
      ],
      templates: [
        { suffix: '', removed: 'る', firstRule: 0, secondRule: null },
        { suffix: 'べ', removed: 'ぶ', firstRule: 1, secondRule: 0 }
      ],
      rootKeys: [
        { route: 'kanji', pos: 'v1', sourceText: '食べる', records: [{
          rootGroup: 0, sourceForm: '食べる', sourceReading: 'たべる', ord: 0, common: 0
        }] },
        { route: 'kanji', pos: 'v5b', sourceText: '食ぶ', records: [{
          rootGroup: 1, sourceForm: '食ぶ', sourceReading: 'たぶ', ord: 0, common: null
        }] }
      ],
      rootGroups: [
        { seq: 900, forms: ['食べる'] },
        { seq: 901, forms: ['食ぶ'] }
      ],
      patches: [],
      tombstones: []
    };
    const support: AnalyzerSupportSource = {
      ...supportSource,
      suffixes: [{ text: 'そう', values: [{ keyword: ':sou', form: {
        seq: 902, text: 'そう', bestKanji: null, commonTags: 'ichi1', ord: 0,
        common: 0, conjugatable: false, nokanji: true, conjugations: ':root'
      } }] }],
      suffixClasses: [{ seq: 902, keyword: ':sou' }],
      collisions: [{
        rootSeq: 901, collisionSeq: 900, viaSeq: null, route: 'kanji',
        surface: '食べる', ruleIds: [1], nKanji: 1, nKana: 0,
        primaryNokanji: false, archived: false, preferKana: false,
        preferKanaOnOrdinalZero: false, pos: ['v1'], skipWord: false,
        finalParticle: false, semiFinalParticle: false, nonFinalParticle: false,
        copula: false, noKanjiBreakPenalty: false
      }],
      generated: {
        ...supportSource.generated,
        ruleAliases: [0, 1],
        aliasCount: 2
      }
    };
    const generated: NonNullable<PortableAnalyzerAnnotations['generated']> = (
      rootSeq, aliases
    ) => {
      if (rootSeq === 900 && aliases.length === 1 && aliases[0] === 0) {
        return { nKanji: 1, nKana: 0, physicalGroup: 77, members: [{
          property: { posId: 0, type: 13, negative: false, formal: false },
          memberOrd: 0, propOrd: 0, viaMemberOrd: null
        }] };
      }
      if (rootSeq === 901 && aliases.length === 2
        && aliases[0] === 1 && aliases[1] === 0) {
        return { nKanji: 1, nKana: 0, physicalGroup: 77, members: [{
          property: { posId: 0, type: 13, negative: false, formal: false },
          memberOrd: 1, propOrd: 0, viaMemberOrd: 0
        }] };
      }
      return null;
    };
    const runtime = analyzer(
      generated,
      undefined,
      morphology,
      roots,
      surfaceBytes(['食べる', '食ぶ', 'そう'], ['食べ']),
      support
    );

    expect(runtime.analyze('食べ', { limit: 1 }).paths[0]?.tokens[0]?.score).toBe(72);
    expect(runtime.analyze('食べそう', { limit: 1 }).paths[0]?.tokens[0]).toMatchObject({
      text: '食べそう',
      score: 223,
      components: [{ text: '食べ', root: { seq: 900 } }, { text: 'そう' }]
    });
  });

  test('omits an indirect conjugation whose original reading chain is missing', async () => {
    const runtime = analyzer();
    const analysis = runtime.analyze('食べた', { limit: 1 });
    const support = openAnalyzerSupport(buildAnalyzerSupport(supportSource).bytes);
    const roots = openRootPayload(buildRootPayload(rootSource).bytes);
    const detailed = await serializePortableLegacyDetailed(
      analysis,
      await detailReader(),
      {
        roots,
        support,
        directSurface: () => '',
        presentationFacts: () => ({
          physicalGroup: 77,
          suffixClass: null,
          definitionSeq: null,
          identityRoots: [200],
          conjugationSelection: 'default',
          semanticMembers: [{
            entryIndex: null,
            root: null,
            inflection: [
              { pos: 'v1', type: 6, negative: false, formal: false, ordinal: 0 },
              { pos: 'v1', type: 2, negative: false, formal: false, ordinal: 1 }
            ],
            stageGroups: [70, 71],
            stageMemberOrds: [0, 0],
            memberOrd: 0
          }]
        })
      }
    );

    expect(firstDetailedWord(detailed).conj).toEqual([]);
  });

  test('expands generated physical properties and binds two-stage via members', async () => {
    const runtime = analyzer((rootSeq, aliases) => {
      if (rootSeq !== 200) return null;
      if (aliases.length === 1 && aliases[0] === 0) {
        return {
          nKanji: 2,
          nKana: 1,
          physicalGroup: 30,
          members: [
            {
              property: { posId: 0, type: 1, negative: true, formal: false },
              memberOrd: 1, propOrd: 0, viaMemberOrd: null
            },
            {
              property: { posId: 0, type: 2, negative: false, formal: false },
              memberOrd: 1, propOrd: 1, viaMemberOrd: null
            }
          ]
        };
      }
      if (aliases.length === 1 && aliases[0] === 1) {
        return {
          nKanji: null,
          nKana: null,
          physicalGroup: 40,
          members: [
            {
              property: { posId: 0, type: 13, negative: false, formal: false },
              memberOrd: 1, propOrd: 0, viaMemberOrd: null
            },
            {
              property: { posId: 0, type: 5, negative: false, formal: false },
              memberOrd: 2, propOrd: 0, viaMemberOrd: null
            }
          ]
        };
      }
      if (aliases.length === 2 && aliases[0] === 1 && aliases[1] === 2) {
        return {
          nKanji: 3,
          nKana: 2,
          physicalGroup: 50,
          members: [
            {
              property: { posId: 0, type: 2, negative: false, formal: false },
              memberOrd: 3, propOrd: 0, viaMemberOrd: 2
            },
            {
              property: { posId: 0, type: 2, negative: false, formal: true },
              memberOrd: 3, propOrd: 1, viaMemberOrd: 2
            }
          ]
        };
      }
      return null;
    });

    const oneStage = runtime.analyze('食べない', { limit: 1 });
    expect(oneStage.paths[0]?.tokens[0]).toMatchObject({
      root: { seq: 200 },
      inflection: [{ pos: 'v1', type: 1, negative: true }]
    });
    const oneStageWord = firstDetailedWord(
      await runtime.serializeLegacyDetailed(oneStage, await detailReader())
    );
    expect(oneStageWord.conj).toHaveLength(1);
    expect(oneStageWord.conj?.[0]?.prop).toEqual([
      { pos: 'v1', type: 'Non-past', neg: true },
      { pos: 'v1', type: 'Past (~ta)' }
    ]);

    const twoStage = runtime.analyze('食べたた', { limit: 1 });
    expect(twoStage.paths[0]?.tokens[0]).toMatchObject({
      root: { seq: 200 },
      inflection: [
        { pos: 'v1', type: 5 },
        { pos: 'v1', type: 2, formal: false }
      ]
    });
    const twoStageWord = firstDetailedWord(
      await runtime.serializeLegacyDetailed(twoStage, await detailReader())
    );
    expect(twoStageWord.conj).toEqual([expect.objectContaining({
      prop: [
        { pos: 'v1', type: 'Past (~ta)' },
        { pos: 'v1', type: 'Past (~ta)', fml: true }
      ],
      via: [expect.objectContaining({
        prop: [{ pos: 'v1', type: 'Potential' }]
      })]
    })]);
  });

  test('keeps an exact prefix overlay when the final generated row is implicit', async () => {
    const runtime = analyzer((rootSeq, aliases) => {
      if (rootSeq !== 200 || aliases.length !== 1 || aliases[0] !== 1) return null;
      return {
        nKanji: null,
        nKana: null,
        physicalGroup: null,
        members: [
          {
            property: { posId: 0, type: 13, negative: false, formal: false },
            memberOrd: 0, propOrd: 0, viaMemberOrd: null
          },
          {
            property: { posId: 0, type: 5, negative: false, formal: false },
            memberOrd: 0, propOrd: 1, viaMemberOrd: null
          }
        ]
      };
    });

    const analysis = runtime.analyze('食べたた', { limit: 1 });
    expect(analysis.paths[0]?.tokens[0]?.inflection).toEqual([
      { pos: 'v1', type: 13, negative: false, formal: false, ordinal: 0 },
      { pos: 'v1', type: 2, negative: false, formal: false, ordinal: 1 }
    ]);
    const word = firstDetailedWord(
      await runtime.serializeLegacyDetailed(analysis, await detailReader())
    );
    expect(word.conj).toEqual([expect.objectContaining({
      prop: [{ pos: 'v1', type: 'Past (~ta)' }],
      via: [expect.objectContaining({
        prop: [
          { pos: 'v1', type: 'Continuative (~i)' },
          { pos: 'v1', type: 'Potential' }
        ]
      })]
    })]);
  });

  test('uses exact semantic lookup ranks for equal-score physical targets', () => {
    const equalMorphology: CompiledMorphologyArtifact = {
      ...morphologySource,
      rootKeys: morphologySource.rootKeys.map(root => ({
        ...root,
        records: root.records.map(record => ({ ...record, common: 1 }))
      }))
    };
    const equalRoots: RootPayloadSource = {
      ...rootSource,
      forms: rootSource.forms.map(form => form.seq === 201
        ? { ...form, common: 1, commonTags: 'ichi1' }
        : form)
    };
    let boundCalls = 0;
    const routedCalls = new Set<string>();
    const lookupOrder: NonNullable<PortableAnalyzerAnnotations['lookupOrder']> = function (
      this: PortableAnalyzerAnnotations,
      route,
      surface,
      rootSeq,
      aliases
    ) {
      if (typeof this.split !== 'function') throw new Error('Lookup rank receiver was not bound');
      routedCalls.add(`${route}:${surface}`);
      boundCalls++;
      if (aliases === null) return rootSeq === 201 ? 0 : rootSeq === 200 ? 1 : null;
      return rootSeq === 201 ? 0 : rootSeq === 200 ? 1 : null;
    };
    const runtime = analyzer(undefined, lookupOrder, equalMorphology, equalRoots);
    const token = runtime.analyze('食べた', { limit: 2 }).paths[0]?.tokens[0];
    expect(token?.root?.seq).toBe(201);
    expect(token?.alternatives.map(value => value.root?.seq)).toEqual([201, 200]);
    expect(boundCalls).toBeGreaterThan(0);
    expect(routedCalls.has('kanji:食べた')).toBeTrue();
    const direct = runtime.analyze('食べる', { limit: 2 }).paths[0]?.tokens[0];
    expect(direct?.root?.seq).toBe(200);
    expect(direct?.alternatives.map(value => value.root?.seq)).toEqual([200, 201]);

    const partial: NonNullable<PortableAnalyzerAnnotations['lookupOrder']> = (
      _route, _surface, rootSeq, aliases
    ) => aliases !== null && rootSeq === 200
      ? 0
      : null;
    expect(() => analyzer(undefined, partial, equalMorphology).analyze('食べた'))
      .toThrow('Incomplete analyzer lookup order');

    const mixedRoots: RootPayloadSource = {
      entries: [...equalRoots.entries, {
        seq: 202, nKanji: 1, nKana: 0, primaryNokanji: false, archived: false,
        preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n']
      }],
      forms: [...equalRoots.forms, {
        surface: '食べた', route: 'kanji', seq: 202, ord: 0, common: 1,
        commonTags: 'ichi1', conjugatable: false, nokanji: false, best: null
      }],
      restrictions: equalRoots.restrictions
    };
    const seen = new Set<string>();
    const mixedOrder: NonNullable<PortableAnalyzerAnnotations['lookupOrder']> = (
      route, surface, rootSeq, aliases
    ) => {
      seen.add(`${route}:${surface}:${rootSeq}:${aliases?.join(',') ?? 'direct'}`);
      if (aliases === null) return rootSeq === 202 ? 0 : null;
      return rootSeq === 201 ? 1 : rootSeq === 200 ? 2 : null;
    };
    analyzer(
      undefined,
      mixedOrder,
      equalMorphology,
      mixedRoots,
      surfaceBytes(['食べた'])
    ).analyze('食べた');
    expect(seen.has('kanji:食べた:202:direct')).toBeTrue();

    const missingDirect: NonNullable<PortableAnalyzerAnnotations['lookupOrder']> = (
      _route, _surface, rootSeq, aliases
    ) => aliases === null ? null : rootSeq === 201 ? 1 : rootSeq === 200 ? 2 : null;
    expect(() => analyzer(
      undefined,
      missingDirect,
      equalMorphology,
      mixedRoots,
      surfaceBytes(['食べた'])
    ).analyze('食べた')).toThrow('Incomplete analyzer lookup order');

    const sharedGenerated: NonNullable<PortableAnalyzerAnnotations['generated']> = (
      rootSeq, aliases
    ) => (rootSeq === 200 || rootSeq === 201)
      && aliases.length === 1 && aliases[0] === 2
      ? { nKanji: null, nKana: null, physicalGroup: 77, members: null }
      : null;
    const conflictingGenerated: NonNullable<PortableAnalyzerAnnotations['lookupOrder']> = (
      _route, _surface, rootSeq, aliases
    ) => aliases === null ? 0 : rootSeq === 200 ? 1 : rootSeq === 201 ? 2 : null;
    expect(() => analyzer(
      sharedGenerated,
      conflictingGenerated,
      equalMorphology,
      mixedRoots,
      surfaceBytes(['食べた'])
    ).analyze('食べた')).toThrow('Physical analyzer group has conflicting lookup orders');

    const collisionRoots: RootPayloadSource = {
      entries: [...equalRoots.entries, {
        seq: 203, nKanji: 0, nKana: 1, primaryNokanji: true, archived: false,
        preferKana: false, preferKanaOnOrdinalZero: false, pos: ['n']
      }],
      forms: [...equalRoots.forms, {
        surface: 'たべた', route: 'kana', seq: 203, ord: 0, common: 1,
        commonTags: 'ichi1', conjugatable: false, nokanji: true, best: null
      }],
      restrictions: equalRoots.restrictions
    };
    const collisionSeen = new Set<string>();
    const collisionOrder: NonNullable<PortableAnalyzerAnnotations['lookupOrder']> = (
      route, surface, rootSeq, aliases
    ) => {
      collisionSeen.add(`${route}:${surface}:${rootSeq}:${aliases?.join(',') ?? 'direct'}`);
      if (aliases !== null) return rootSeq === 200 ? 1 : rootSeq === 201 ? 2 : null;
      return rootSeq === 203 ? 0 : rootSeq === 900 ? 1 : null;
    };
    analyzer(
      undefined,
      collisionOrder,
      equalMorphology,
      collisionRoots,
      surfaceBytes(['たべた'])
    ).analyze('たべた');
    expect(collisionSeen.has('kana:たべた:900:direct')).toBeTrue();
    expect(collisionSeen.has('kana:たべた:200:2')).toBeFalse();
    expect(collisionSeen.has('kana:たべた:201:2')).toBeFalse();
  });
});
