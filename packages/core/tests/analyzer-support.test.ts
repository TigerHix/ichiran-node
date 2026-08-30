import { describe, expect, test } from 'bun:test';
import {
  buildAnalyzerSupport,
  type AnalyzerSupportSource
} from '../../data/src/browser-pack/analyzer-support.js';
import {
  ANALYZER_SUPPORT_FORMAT_VERSION,
  AnalyzerSupportFormatError,
  openAnalyzerSupport
} from '../src/analyzer-support.js';

const source: AnalyzerSupportSource = {
  suffixes: [
    {
      text: 'た',
      values: [{
        keyword: ':tai',
        form: {
          seq: 900000,
          text: 'たそう',
          bestKanji: null,
          commonTags: '',
          ord: 0,
          common: null,
          conjugatable: false,
          nokanji: true,
          conjugations: [{
            seq: 3000001,
            from: 2017560,
            via: null,
            pos: 'adj-i',
            type: 13,
            negative: false,
            formal: null
          }]
        }
      }]
    },
    {
      text: 'ちゃ',
      values: [
        { keyword: ':teba', form: null },
        { keyword: ':chau', form: null }
      ]
    }
  ],
  suffixClasses: [
    { seq: 900000, keyword: ':tasou' },
    { seq: 2028920, keyword: ':ha' }
  ],
  counters: [
    {
      key: '本', order: 0, className: 'CounterText', text: '本', kana: 'ほん', suffix: null,
      source: { seq: 1455650, route: 'kanji', text: '本', ord: 0 },
      ordinal: false, foreign: false, common: null, suffixDescriptions: [],
      digitOptions: [[3, ':r'], [10]], digitSet: [], allowed: []
    },
    {
      key: '本目', order: 0, className: 'CounterText', text: '本目', kana: 'ほん', suffix: 'め',
      source: { seq: 1455650, route: 'kanji', text: '本', ord: 0 },
      ordinal: true, foreign: false, common: 0, suffixDescriptions: ['[ordinal]'],
      digitOptions: [[3, ':r']], digitSet: [1, 2], allowed: [1, 2, 3]
    }
  ],
  splits: [
    {
      definitionSeq: 1008450, route: 'kana', surface: 'では', kind: 'segsplit',
      parts: [
        { seq: 2028980, route: 'kana', text: 'で', best: null, ord: 0, common: 0,
          commonTags: 'ichi1', conjugatable: false, nokanji: true },
        { seq: 2028920, route: 'kana', text: 'は', best: null, ord: 0, common: 0,
          commonTags: 'ichi1', conjugatable: false, nokanji: true }
      ],
      score: -5, primary: 0, connector: ' ', root: []
    },
    {
      definitionSeq: 123, route: 'kanji', surface: '例', kind: 'split',
      parts: [':score'], score: 20, primary: 0, connector: '', root: [1]
    }
  ],
  hints: [
    { definitionSeq: 1008450, route: 'kana', surface: 'では', reading: 'では', hint: 'で\u200cは' }
  ],
  collisions: [
    {
      rootSeq: 1000260, collisionSeq: 1000890, route: 'kanji', surface: '悪どかった',
      viaSeq: null,
      ruleIds: [3], nKanji: 1, nKana: 1, primaryNokanji: false, archived: true,
      preferKana: true, preferKanaOnOrdinalZero: false, pos: ['adj-i', 'exp'],
      skipWord: false, finalParticle: false, semiFinalParticle: false,
      nonFinalParticle: false, copula: false, noKanjiBreakPenalty: true
    }
  ],
  generated: {
    ruleAliases: [2, 0, 1],
    aliasCount: 3,
    records: [],
    semanticPaths: 0,
    countExceptions: 0,
    physicalGroups: 0,
    physicalMembers: 0
  }
};

describe('analyzer support pack', () => {
  test('round-trips suffixes, counters, annotations, and collision facts', () => {
    const build = buildAnalyzerSupport(source);
    const reversed = buildAnalyzerSupport({
      suffixes: [...source.suffixes].reverse(),
      suffixClasses: [...source.suffixClasses].reverse(),
      counters: [...source.counters].reverse(),
      splits: [...source.splits].reverse(),
      hints: [...source.hints].reverse(),
      collisions: [...source.collisions].reverse(),
      generated: source.generated
    });
    expect(reversed.bytes).toEqual(build.bytes);

    const reader = openAnalyzerSupport(build.bytes);
    expect(reader.suffix('た')).toEqual([{
      keyword: ':tai',
      form: {
        ...source.suffixes[0]!.values[0]!.form,
        conjugations: [{
          seq: 3000001,
          from: 2017560,
          via: null,
          property: { pos: 'adj-i', type: 13, negative: false, formal: null }
        }]
      }
    }]);
    expect(reader.suffix('ちゃ')).toEqual(source.suffixes[1]!.values);
    expect(reader.suffix('missing')).toEqual([]);
    expect(reader.suffixMatchesEndingAt('xたちゃ', 4)).toEqual([
      { start: 2, end: 4, text: 'ちゃ', values: source.suffixes[1]!.values },
      { start: 1, end: 4, text: 'たちゃ', values: [] }
    ].filter(value => value.values.length > 0));
    expect(reader.suffixMatchesEndingAt('xたちゃ', 4, 1)).toEqual([]);
    expect(reader.suffixClass(900000)).toBe(':tasou');
    expect(reader.suffixClass(1)).toBeNull();
    expect(reader.counters('本')).toEqual([{
      ...source.counters[0],
      key: undefined,
      order: undefined
    }].map(({ key: _key, order: _order, ...value }) => value));
    expect(reader.counters('本目')).toEqual([{
      ...source.counters[1],
      key: undefined,
      order: undefined
    }].map(({ key: _key, order: _order, ...value }) => value));
    expect(reader.counterMatchesStartingAt('3本目先', 1)).toEqual([
      { start: 1, end: 3, text: '本目', values: reader.counters('本目') },
      { start: 1, end: 2, text: '本', values: reader.counters('本') }
    ]);
    expect(reader.counterMatchesStartingAt('3本目先', 1, 1)).toEqual([
      { start: 1, end: 2, text: '本', values: reader.counters('本') }
    ]);
    expect(reader.counters('missing')).toEqual([]);
    expect(reader.split(1008450, 'kana', 'では', 'segsplit')).toEqual(source.splits[0]);
    expect(reader.split(123, 'kanji', '例')).toEqual(source.splits[1]);
    expect(reader.hint(1008450, 'kana', 'では', 'では')).toBe('で\u200cは');
    expect(reader.hint(1008450, 'kana', 'では', 'でわ')).toBeNull();
    expect(reader.collision(1000260, 'kanji', '悪どかった', [3])).toEqual(source.collisions[0]);
    expect(reader.generatedAliases([0])).toEqual([2]);
    expect(reader.generatedAliases([1, 2])).toEqual([0, 1]);
    expect(reader.stats.generatedRules).toBe(3);
    expect(reader.stats.generatedAliases).toBe(3);
  });

  test('rejects corruption and unsupported versions', () => {
    const encoded = buildAnalyzerSupport(source).bytes;
    const badVersion = encoded.slice();
    new DataView(badVersion.buffer).setUint16(8, ANALYZER_SUPPORT_FORMAT_VERSION + 1, true);
    expect(() => openAnalyzerSupport(badVersion)).toThrow(AnalyzerSupportFormatError);

    const badPayload = encoded.slice();
    badPayload[badPayload.length - 1] ^= 1;
    expect(() => openAnalyzerSupport(badPayload)).toThrow(AnalyzerSupportFormatError);
  });
});
