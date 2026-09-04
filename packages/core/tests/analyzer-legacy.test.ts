import { gunzipSync } from 'node:zlib';
import { createHash } from 'node:crypto';
import { describe, expect, test } from 'bun:test';
import { buildLexiconStore } from '../../data/src/browser-pack/lexicon.js';
import { buildLocaleGlossStore } from '../../data/src/browser-pack/locale-gloss.js';
import {
  buildRootPayload,
  compareRootPayloadText,
  type RootPayloadSource
} from '../../data/src/browser-pack/root-payload.js';
import {
  serializePortableLegacyDetailed,
  type PortableLegacyGlossJson,
  type PortableLegacyPresentationFacts
} from '../src/analyzer-legacy.js';
import type { AnalyzerSupportReader } from '../src/analyzer-support.js';
import {
  DictionaryReader,
  LexiconStoreReader,
  LocaleGlossStoreReader,
  memoryDictionarySource
} from '../src/dictionary.js';
import type { DictionaryEntry } from '../src/dictionary-contract.js';
import { openRootPayload } from '../src/root-payload.js';
import type {
  PortableAnalysisComponent,
  PortableAnalysisResult,
  PortableAnalysisToken
} from '../src/analyzer.js';

const roots: RootPayloadSource = {
  entries: [
    {
      seq: 100,
      nKanji: 2,
      nKana: 3,
      primaryNokanji: false,
      archived: false,
      preferKana: false,
      preferKanaOnOrdinalZero: false,
      pos: ['v5k', 'n']
    },
    {
      seq: 300,
      nKanji: 0,
      nKana: 1,
      primaryNokanji: true,
      archived: false,
      preferKana: false,
      preferKanaOnOrdinalZero: false,
      pos: ['aux-adj']
    },
    {
      seq: 400,
      nKanji: 3,
      nKana: 1,
      primaryNokanji: false,
      archived: false,
      preferKana: false,
      preferKanaOnOrdinalZero: false,
      pos: ['v1']
    }
  ],
  forms: [
    {
      surface: '書く', route: 'kanji', seq: 100, ord: 0, common: 1,
      commonTags: 'ichi1', conjugatable: true, nokanji: false, best: 'かく'
    },
    {
      surface: '描く', route: 'kanji', seq: 100, ord: 1, common: 2,
      commonTags: '', conjugatable: true, nokanji: false, best: 'えがく'
    },
    {
      surface: 'かく', route: 'kana', seq: 100, ord: 0, common: 1,
      commonTags: 'ichi1', conjugatable: true, nokanji: false, best: '書く'
    },
    {
      surface: 'えがく', route: 'kana', seq: 100, ord: 1, common: 2,
      commonTags: '', conjugatable: true, nokanji: false, best: '描く'
    },
    {
      surface: 'なぞる', route: 'kana', seq: 100, ord: 2, common: null,
      commonTags: '', conjugatable: true, nokanji: true, best: null
    },
    {
      surface: 'たい', route: 'kana', seq: 300, ord: 0, common: 1,
      commonTags: 'ichi1', conjugatable: false, nokanji: true, best: null
    },
    {
      surface: '上げる', route: 'kanji', seq: 400, ord: 0, common: 1,
      commonTags: 'ichi1', conjugatable: true, nokanji: false, best: 'あげる'
    },
    {
      surface: '揚げる', route: 'kanji', seq: 400, ord: 1, common: 2,
      commonTags: '', conjugatable: true, nokanji: false, best: 'あげる'
    },
    {
      surface: '挙げる', route: 'kanji', seq: 400, ord: 2, common: 3,
      commonTags: '', conjugatable: true, nokanji: false, best: 'あげる'
    },
    {
      surface: 'あげる', route: 'kana', seq: 400, ord: 0, common: 1,
      commonTags: 'ichi1', conjugatable: true, nokanji: false, best: '上げる'
    }
  ],
  restrictions: [
    { seq: 100, reading: 'かく', written: '書く' },
    { seq: 100, reading: 'えがく', written: '描く' }
  ]
};

const details: readonly DictionaryEntry[] = [
  {
    seq: 100,
    forms: roots.forms
      .filter(value => value.seq === 100)
      .map(({ surface, ...value }) => ({ ...value, text: surface })),
    senses: [
      {
        ord: 0,
        glosses: [{ ord: 0, text: 'generic action' }],
        properties: [{ tag: 'pos', ord: 0, text: 'v5k' }]
      },
      {
        ord: 1,
        glosses: [{ ord: 0, text: 'to write' }],
        properties: [
          { tag: 'pos', ord: 0, text: 'v5k' },
          { tag: 'stagk', ord: 0, text: '書く' }
        ]
      },
      {
        ord: 2,
        glosses: [{ ord: 0, text: 'to draw' }],
        properties: [
          { tag: 'pos', ord: 0, text: 'v5k' },
          { tag: 'stagr', ord: 0, text: 'えがく' }
        ]
      },
      {
        ord: 3,
        glosses: [{ ord: 0, text: 'a written work' }],
        properties: [
          { tag: 'pos', ord: 0, text: 'n' },
          { tag: 'pos', ord: 1, text: 'adj-no' }
        ]
      }
    ]
  },
  {
    seq: 300,
    forms: roots.forms
      .filter(value => value.seq === 300)
      .map(({ surface, ...value }) => ({ ...value, text: surface })),
    senses: [{
      ord: 0,
      glosses: [{ ord: 0, text: 'desire auxiliary' }],
      properties: [{ tag: 'pos', ord: 0, text: 'aux-adj' }]
    }]
  },
  {
    seq: 400,
    forms: roots.forms
      .filter(value => value.seq === 400)
      .map(({ surface, ...value }) => ({ ...value, text: surface })),
    senses: [
      {
        ord: 0,
        glosses: [{ ord: 0, text: 'generic raising' }],
        properties: [{ tag: 'pos', ord: 0, text: 'v1' }]
      },
      {
        ord: 1,
        glosses: [{ ord: 0, text: 'raise something' }],
        properties: [
          { tag: 'pos', ord: 0, text: 'v1' },
          { tag: 'stagk', ord: 0, text: '上げる' }
        ]
      },
      {
        ord: 2,
        glosses: [{ ord: 0, text: 'deep-fry' }],
        properties: [
          { tag: 'pos', ord: 0, text: 'v1' },
          { tag: 'stagk', ord: 0, text: '揚げる' }
        ]
      },
      {
        ord: 3,
        glosses: [{ ord: 0, text: 'nominate' }],
        properties: [
          { tag: 'pos', ord: 0, text: 'v1' },
          { tag: 'stagk', ord: 0, text: '挙げる' }
        ]
      }
    ]
  }
];

const directSurfaces = [...new Set(roots.forms.map(value => value.surface))]
  .sort(compareRootPayloadText);
const rootReader = openRootPayload(buildRootPayload(roots).bytes);
// Detailed presentation only needs the exact suffix-class lookup. Keeping this
// fixture structural avoids coupling a cold-view test to the full hot compiler.
const supportReader = {
  suffixClass: (seq: number) => seq === 200 ? ':tai' : null
} as unknown as AnalyzerSupportReader;

async function detailReader(): Promise<DictionaryReader> {
  const lexiconBytes = buildLexiconStore(details.map(entry => ({
    seq: entry.seq,
    forms: entry.forms,
    senses: entry.senses.map(sense => ({
      ord: sense.ord,
      properties: sense.properties.filter(property => property.tag !== 's_inf')
    }))
  })), { targetBlockBytes: 1024 }).bytes;
  const sha256 = createHash('sha256').update(lexiconBytes).digest('hex');
  const localeBytes = buildLocaleGlossStore({
    locale: 'en',
    lexiconSha256: sha256,
    entries: details.map(entry => ({
      seq: entry.seq,
      groups: entry.senses.map(sense => ({
        targets: [sense.ord],
        glosses: sense.glosses,
        info: sense.properties.filter(property => property.tag === 's_inf')
          .map(({ ord, text }) => ({ ord, text }))
      }))
    })),
    targetBlockBytes: 1024
  }).bytes;
  const decode = async (compressed: Uint8Array) => new Uint8Array(gunzipSync(compressed));
  const lexicon = await LexiconStoreReader.open(memoryDictionarySource(lexiconBytes), decode);
  const locale = await LocaleGlossStoreReader.open(memoryDictionarySource(localeBytes), decode, {
    locale: 'en', lexiconSha256: sha256, entryCount: details.length
  });
  return new DictionaryReader(lexicon, locale, locale);
}

function token(overrides: Partial<PortableAnalysisToken>): PortableAnalysisToken {
  return {
    candidateId: 1,
    start: 0,
    end: 2,
    text: '書く',
    trueText: null,
    route: 'kanji',
    reading: 'かく',
    romanized: 'kaku',
    pos: ['v5k'],
    score: 100,
    entryIndex: 0,
    root: { seq: 100, form: '書く', reading: 'かく' },
    inflection: [],
    components: [],
    alternatives: [],
    skipped: 0,
    entity: false,
    counter: null,
    ...overrides
  };
}

function result(value: PortableAnalysisToken): PortableAnalysisResult {
  const path = { score: value.score, tokens: [value] };
  return {
    input: value.text,
    normalized: value.text,
    computeMs: 0,
    chunks: [{ type: 'word', start: 0, end: value.text.length, text: value.text, paths: [path] }],
    paths: [path]
  };
}

async function render(
  value: PortableAnalysisToken,
  facts?: ReadonlyMap<object, PortableLegacyPresentationFacts>,
  hint?: (definitionSeq: number, route: 'kana' | 'kanji', surface: string) => string | null
): Promise<PortableLegacyGlossJson> {
  const transformed = await serializePortableLegacyDetailed(
    result(value),
    await detailReader(),
    {
      roots: rootReader,
      support: supportReader,
      directSurface: rank => directSurfaces[rank]!,
      hint: (definitionSeq, route, surface) => hint?.(definitionSeq, route, surface) ?? null,
      presentationFacts: candidate => facts?.get(candidate) ?? null
    }
  );
  const chunks = transformed as readonly (readonly [
    readonly (readonly [string, PortableLegacyGlossJson, unknown])[],
    number
  ][])[];
  return chunks[0]![0]![0]![0]![1];
}

function glosses(value: PortableLegacyGlossJson): string[] {
  return value.gloss?.map(sense => sense.gloss) ?? [];
}

describe('legacy detailed presentation', () => {
  test('ports written/reading restrictions and nokanji crossing exactly', async () => {
    const written = await render(token({}));
    expect(glosses(written)).toEqual(['generic action', 'to write', 'a written work']);
    expect(written.gloss?.at(-1)?.pos).toBe('[adj-no,n]');

    const drawn = await render(token({
      text: '描く',
      reading: 'えがく',
      root: { seq: 100, form: '描く', reading: 'えがく' }
    }));
    expect(glosses(drawn)).toEqual(['generic action', 'to draw', 'a written work']);

    const kana = await render(token({
      text: 'かく',
      route: 'kana',
      reading: 'かく',
      root: { seq: 100, form: 'かく', reading: 'かく' }
    }));
    expect(glosses(kana)).toEqual(['generic action', 'to write', 'a written work']);

    const noKanji = await render(token({
      text: 'なぞる',
      end: 3,
      route: 'kana',
      reading: 'なぞる',
      root: { seq: 100, form: 'なぞる', reading: 'なぞる' }
    }));
    expect(glosses(noKanji)).toEqual(['generic action', 'a written work']);
  });

  test('filters inflected senses by conjugation POS and omits top-level gloss', async () => {
    const inflected = await render(token({
      text: '書いた',
      end: 3,
      reading: 'かいた',
      inflection: [{
        pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
      }]
    }));
    expect(inflected.gloss).toBeUndefined();
    expect(inflected.conj).toEqual([{
      prop: [{ pos: 'v5k', type: 'Past (~ta)' }],
      reading: '書く 【かく】',
      gloss: [
        { pos: '[v5k]', gloss: 'generic action' },
        { pos: '[v5k]', gloss: 'to write' }
      ],
      readok: true
    }]);

    const entity = await render(token({
      text: '書いた',
      end: 3,
      reading: 'かいた',
      entity: true,
      inflection: [{
        pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
      }]
    }));
    expect(entity.gloss).toEqual([
      { pos: '[n-pr]', gloss: 'proper noun (named entity)' }
    ]);
  });

  test('uses the kana source row for inflected sense restrictions while keeping its best-kanji label', async () => {
    const visible = token({
      text: 'あげよう',
      end: 5,
      route: 'kana',
      reading: 'あげよう',
      entryIndex: 2,
      root: { seq: 400, form: '上げる', reading: 'あげる' },
      inflection: [{
        pos: 'v1', type: 9, negative: false, formal: false, ordinal: 0
      }]
    });
    const facts = new Map<object, PortableLegacyPresentationFacts>([[visible, {
      physicalGroup: null,
      suffixClass: null,
      definitionSeq: 400,
      semanticMembers: [{
        entryIndex: 2,
        root: { seq: 400, form: '上げる', reading: 'あげる' },
        inflection: [{
          pos: 'v1', type: 9, negative: false, formal: false, ordinal: 0
        }],
        stageGroups: [null],
        stageMemberOrds: [null],
        stagePropOrds: [null],
        memberOrd: null
      }]
    }]]);

    const rendered = await render(visible, facts);
    expect(rendered.conj?.[0]?.reading).toBe('上げる 【あげる】');
    expect(rendered.conj?.[0]?.gloss.map(value => value.gloss)).toEqual([
      'generic raising', 'raise something', 'deep-fry', 'nominate'
    ]);
  });

  test('reconstructs one physical target with nested semantic roots', async () => {
    const visible = token({
      text: '書かせられ',
      end: 5,
      reading: 'かかせられ',
      inflection: [
        { pos: 'v5k', type: 8, negative: false, formal: false, ordinal: 0 },
        { pos: 'v1', type: 13, negative: false, formal: false, ordinal: 0 }
      ]
    });
    const facts = new Map<object, PortableLegacyPresentationFacts>([[visible, {
      physicalGroup: 20,
      suffixClass: null,
      definitionSeq: null,
      semanticMembers: [
        {
          entryIndex: 0,
          root: { seq: 100, form: '書く', reading: 'かく' },
          inflection: [
            { pos: 'v5k', type: 8, negative: false, formal: false, ordinal: 0 },
            { pos: 'v1', type: 13, negative: false, formal: false, ordinal: 0 }
          ],
          stageGroups: [10, 20],
          stageMemberOrds: [1, 5],
          stagePropOrds: [0, 0],
          memberOrd: 5
        },
        {
          entryIndex: 1,
          root: { seq: 300, form: 'たい', reading: 'たい' },
          inflection: [
            { pos: 'aux-adj', type: 6, negative: false, formal: false, ordinal: 0 },
            { pos: 'v1', type: 13, negative: false, formal: false, ordinal: 0 }
          ],
          stageGroups: [10, 20],
          stageMemberOrds: [2, 6],
          stagePropOrds: [0, 0],
          memberOrd: 6
        }
      ]
    }]]);

    const grouped = await render(visible, facts);
    expect(grouped.seq).toEqual([100, 300]);
    expect(grouped.gloss).toBeUndefined();
    expect(grouped.conj).toEqual([{
      prop: [{ pos: 'v1', type: 'Continuative (~i)' }],
      via: [
        {
          prop: [{ pos: 'aux-adj', type: 'Passive' }],
          reading: 'たい',
          gloss: [{ pos: '[aux-adj]', gloss: 'desire auxiliary' }],
          readok: true
        },
        {
          prop: [{ pos: 'v5k', type: 'Causative-Passive' }],
          reading: '書く 【かく】',
          gloss: [
            { pos: '[v5k]', gloss: 'generic action' },
            { pos: '[v5k]', gloss: 'to write' }
          ],
          readok: true
        }
      ],
      readok: true
    }]);
  });

  test('coalesces one physical conjugation row and preserves property order', async () => {
    const visible = token({
      text: '書かれ',
      end: 3,
      reading: 'かかれ',
      inflection: [{
        pos: 'v5k', type: 8, negative: false, formal: false, ordinal: 0
      }]
    });
    const common = {
      entryIndex: 0,
      root: { seq: 100, form: '書く', reading: 'かく' },
      stageGroups: [10],
      stageMemberOrds: [1],
      memberOrd: 1
    } as const;
    const facts = new Map<object, PortableLegacyPresentationFacts>([[visible, {
      physicalGroup: 10,
      suffixClass: null,
      definitionSeq: null,
      semanticMembers: [
        {
          ...common,
          inflection: [{
            pos: 'v5k', type: 8, negative: false, formal: false, ordinal: 0
          }],
          stagePropOrds: [1]
        },
        {
          ...common,
          inflection: [{
            pos: 'v5k', type: 6, negative: false, formal: false, ordinal: 0
          }],
          stagePropOrds: [0]
        }
      ]
    }]]);

    const grouped = await render(visible, facts);
    expect(grouped.conj).toHaveLength(1);
    expect(grouped.conj?.[0]?.prop).toEqual([
      { pos: 'v5k', type: 'Passive' },
      { pos: 'v5k', type: 'Causative-Passive' }
    ]);
  });

  test('uses analyzer hints in semantic-root conjugation labels', async () => {
    const visible = token({
      text: '書いた',
      end: 3,
      reading: 'かいた',
      inflection: [{
        pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
      }]
    });
    const facts = new Map<object, PortableLegacyPresentationFacts>([[visible, {
      physicalGroup: null,
      suffixClass: null,
      definitionSeq: 100,
      semanticMembers: [{
        entryIndex: 0,
        root: { seq: 100, form: '書く', reading: 'かく' },
        inflection: [{
          pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
        }],
        stageGroups: [null],
        stageMemberOrds: [null],
        stagePropOrds: [null],
        memberOrd: null
      }]
    }]]);

    const grouped = await render(visible, facts, (seq, route, surface) =>
      seq === 100 && route === 'kana' && surface === 'かく' ? 'か‌く' : null);
    expect(grouped.conj?.[0]?.reading).toBe('書く 【か‌く】');
  });

  test('scopes pinned kana conjugation source spellings to conjugation leaves', async () => {
    const inflection = [{
      pos: 'vk', type: 2, negative: false, formal: false, ordinal: 0
    }] as const;
    const inflected = (
      seq: number,
      route: 'kana' | 'kanji',
      text: string,
      reading: string,
      form: string,
      rootReading: string,
      pos: string
    ) => token({
      text,
      route,
      reading,
      entryIndex: null,
      root: { seq, form, reading: rootReading },
      inflection: [{ ...inflection[0], pos }]
    });

    expect((await render(inflected(
      1_547_720, 'kana', 'きた', 'きた', '来る', 'くる', 'vk'
    ))).conj?.[0]?.reading).toBe('来る 【クる】');
    expect((await render(inflected(
      1_547_720, 'kanji', '来た', 'きた', '来る', 'くる', 'vk'
    ))).conj?.[0]?.reading).toBe('来る 【くる】');

    expect((await render(inflected(
      2_827_915, 'kana', 'おけばよかった', 'おけばよかった',
      '置けばいい', 'おけばいい', 'adj-ix'
    ))).conj?.[0]?.reading).toBe('置けばよい 【おけばよい】');
    expect((await render(inflected(
      2_827_915, 'kanji', '置けばよかった', 'おけばよかった',
      '置けばいい', 'おけばいい', 'adj-ix'
    ))).conj?.[0]?.reading).toBe('置けばいい 【おけばいい】');

    const directCome = await render(token({
      text: '来る',
      reading: 'くる',
      entryIndex: null,
      root: { seq: 1_547_720, form: '来る', reading: 'くる' }
    }));
    expect(directCome.reading).toBe('来る 【くる】');
    expect(directCome.conj).toEqual([]);
  });

  test('uses direct rows when a generated target also has via rows', async () => {
    const visible = token({
      text: '書けた',
      end: 3,
      reading: 'かけた',
      inflection: [{
        pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
      }]
    });
    const facts = new Map<object, PortableLegacyPresentationFacts>([[visible, {
      physicalGroup: 20,
      suffixClass: null,
      definitionSeq: null,
      semanticMembers: [
        {
          entryIndex: 0,
          root: { seq: 100, form: '書く', reading: 'かく' },
          inflection: [{
            pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
          }],
          stageGroups: [20],
          stageMemberOrds: [1],
          stagePropOrds: [0],
          memberOrd: 1
        },
        {
          entryIndex: 1,
          root: { seq: 300, form: 'たい', reading: 'たい' },
          inflection: [
            { pos: 'aux-adj', type: 5, negative: false, formal: false, ordinal: 0 },
            { pos: 'v1', type: 2, negative: false, formal: false, ordinal: 0 }
          ],
          stageGroups: [10, 20],
          stageMemberOrds: [2, 3],
          stagePropOrds: [0, 0],
          memberOrd: 3
        }
      ]
    }]]);

    const grouped = await render(visible, facts);
    expect(grouped.seq).toEqual([100, 300]);
    expect(grouped.conj).toHaveLength(1);
    expect(grouped.conj?.[0]).toMatchObject({
      prop: [{ pos: 'v5k', type: 'Past (~ta)' }],
      reading: '書く 【かく】'
    });
    expect(grouped.conj?.[0]?.via).toBeUndefined();
  });

  test('honors private default, explicit, and root conjugation selection', async () => {
    const visible = token({
      text: '書けた',
      end: 3,
      reading: 'かけた',
      inflection: [{
        pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
      }]
    });
    const semanticMembers = [
      {
        entryIndex: 0,
        root: { seq: 100, form: '書く', reading: 'かく' },
        inflection: [{
          pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
        }],
        stageGroups: [20],
        stageMemberOrds: [1],
        stagePropOrds: [0],
        memberOrd: 1
      },
      {
        entryIndex: 1,
        root: { seq: 300, form: 'たい', reading: 'たい' },
        inflection: [
          { pos: 'aux-adj', type: 5, negative: false, formal: false, ordinal: 0 },
          { pos: 'v1', type: 2, negative: false, formal: false, ordinal: 0 }
        ],
        stageGroups: [10, 20],
        stageMemberOrds: [2, 3],
        stagePropOrds: [0, 0],
        memberOrd: 3
      }
    ] as const;
    const selected = async (
      conjugationSelection: 'default' | 'explicit' | 'root'
    ): Promise<PortableLegacyGlossJson> => render(visible, new Map([[visible, {
      physicalGroup: 20,
      suffixClass: null,
      definitionSeq: null,
      semanticMembers,
      conjugationSelection
    }]]));

    const ordinary = await selected('default');
    expect(ordinary.conj).toHaveLength(1);
    expect(ordinary.conj?.[0]).toMatchObject({
      prop: [{ pos: 'v5k', type: 'Past (~ta)' }],
      reading: '書く 【かく】'
    });
    expect(ordinary.conj?.[0]?.via).toBeUndefined();

    const explicit = await selected('explicit');
    expect(explicit.conj).toHaveLength(2);
    expect(explicit.conj?.[1]).toMatchObject({
      prop: [{ pos: 'v1', type: 'Past (~ta)' }],
      via: [{ reading: 'たい' }]
    });

    expect((await selected('root')).conj).toEqual([]);
  });

  test('omits a physical root gloss for an explicit semantic conjugation', async () => {
    const visible = token({ inflection: [] });
    const semanticMembers = [{
      entryIndex: 0,
      root: { seq: 100, form: '書く', reading: 'かく' },
      inflection: [{
        pos: 'v5k', type: 2, negative: false, formal: false, ordinal: 0
      }],
      stageGroups: [20],
      stageMemberOrds: [1],
      stagePropOrds: [0],
      memberOrd: 1
    }] as const;
    const selected = async (
      conjugationSelection: 'explicit' | 'root'
    ): Promise<PortableLegacyGlossJson> => render(visible, new Map([[visible, {
      physicalGroup: 20,
      suffixClass: null,
      definitionSeq: null,
      semanticMembers,
      conjugationSelection
    }]]));

    const explicit = await selected('explicit');
    expect(explicit.gloss).toBeUndefined();
    expect(explicit.conj).toHaveLength(1);

    const root = await selected('root');
    expect(root.gloss).toEqual([
      { pos: '[v5k]', gloss: 'generic action' },
      { pos: '[v5k]', gloss: 'to write' },
      { pos: '[adj-no,n]', gloss: 'a written work' }
    ]);
    expect(root.conj).toEqual([]);
  });

  test('retains an uninflected component gloss without presentation facts', async () => {
    const component: PortableAnalysisComponent = {
      text: '書く', trueText: null, route: 'kanji', reading: 'かく',
      entryIndex: 0, root: { seq: 100, form: '書く', reading: 'かく' },
      inflection: [], primary: true
    };
    const compound = await render(token({ components: [component] }));

    expect(compound.components?.[0]?.gloss).toEqual([
      { pos: '[v5k]', gloss: 'generic action' },
      { pos: '[v5k]', gloss: 'to write' },
      { pos: '[adj-no,n]', gloss: 'a written work' }
    ]);
    expect(compound.components?.[0]?.conj).toEqual([]);
  });

  test('uses hidden physical suffix identity only on non-primary components', async () => {
    const primary: PortableAnalysisComponent = {
      text: '書き', trueText: null, route: 'kanji', reading: 'かき',
      entryIndex: 0, root: { seq: 100, form: '書く', reading: 'かく' },
      inflection: [], primary: true
    };
    const suffix: PortableAnalysisComponent = {
      text: 'たい', trueText: null, route: 'kana', reading: 'たい',
      entryIndex: 1, root: { seq: 300, form: 'たい', reading: 'たい' },
      inflection: [], primary: false
    };
    const facts = new Map<object, PortableLegacyPresentationFacts>([[suffix, {
      physicalGroup: null,
      suffixClass: null,
      definitionSeq: 200
    }]]);
    const compound = await render(token({
      text: '書きたい', end: 4, reading: 'かきたい', components: [primary, suffix]
    }), facts);
    expect(compound.compound).toEqual(['書き', 'たい']);
    expect(compound.seq).toBeUndefined();
    expect(compound.suffix).toBeUndefined();
    expect(compound.components?.[0]?.suffix).toBeUndefined();
    expect(compound.components?.[1]).toMatchObject({
      seq: 300,
      suffix: 'want to... / would like to...',
      conj: []
    });
    expect(compound.components?.[1]?.gloss).toBeUndefined();
  });

  test('does not append kana to a bare NumberText reading label', async () => {
    const number = await render(token({
      candidateId: null,
      end: 1,
      text: '1',
      route: 'kana',
      reading: 'いち',
      romanized: '1',
      pos: [],
      entryIndex: null,
      root: null,
      counter: ['Value: 1', false]
    }));
    expect(number).toMatchObject({
      reading: '1',
      text: '1',
      kana: 'いち',
      counter: { value: 'Value: 1', ordinal: [] }
    });
    expect(number.seq).toBeUndefined();
  });

  test('matches synthetic KanaText entity presentation and omits seq/conj', async () => {
    const entity = await render(token({
      candidateId: null,
      text: '未知',
      route: 'kanji',
      reading: '未知',
      romanized: '未知',
      pos: ['proper-noun'],
      entryIndex: null,
      root: null,
      entity: true
    }));
    expect(entity).toEqual({
      reading: '未知',
      text: '未知',
      kana: '未知',
      score: 100,
      gloss: [{ pos: '[n-pr]', gloss: 'proper noun (named entity)' }]
    });
  });
});
