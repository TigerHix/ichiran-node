import { describe, expect, test } from 'bun:test';
import {
  PORTABLE_LEGACY_INFO,
  type IchiranRuntime,
  type PortableLegacyConjugationInfoFacts,
  type PortableLegacyGlossJson,
  type PortableLegacyWordInfoFacts
} from '@ichiran/core';
import { formatLegacyWordInfo, romanizeWithInfo } from '../src/compat.js';
import { openNodeRuntime } from '../src/index.js';

function word(value: PortableLegacyGlossJson): PortableLegacyGlossJson {
  return value;
}

function wordInfo(
  value: PortableLegacyGlossJson,
  facts: PortableLegacyWordInfoFacts
): PortableLegacyGlossJson {
  Object.defineProperty(value, PORTABLE_LEGACY_INFO, { value: facts });
  return value;
}

function conjugationInfo<T extends NonNullable<PortableLegacyGlossJson['conj']>[number]>(
  value: T,
  facts: PortableLegacyConjugationInfoFacts
): T {
  Object.defineProperty(value, PORTABLE_LEGACY_INFO, { value: facts });
  return value;
}

describe('legacy -i word formatter', () => {
  test('formats senses while carrying POS and preserving field/info punctuation', () => {
    expect(formatLegacyWordInfo(word({
      reading: '一本 【いっぽん】',
      text: '一本',
      seq: 1,
      gloss: [
        { pos: '[n]', gloss: 'one long object', field: '{math}', info: 'often figurative' },
        { pos: '[]', gloss: 'one copy' }
      ],
      conj: []
    }))).toBe([
      '一本 【いっぽん】',
      '1. [n] {math} 《often figurative》 one long object',
      '2. [n] one copy'
    ].join('\n'));
  });

  test('formats compounds recursively and marks each component', () => {
    expect(formatLegacyWordInfo(word({
      reading: '食べすぎ 【たべすぎ】',
      text: '食べすぎ',
      compound: ['食べ', 'すぎ'],
      components: [
        {
          reading: '食べる 【たべる】',
          text: '食べ',
          seq: 2,
          gloss: [{ pos: '[v1]', gloss: 'to eat' }],
          conj: []
        },
        {
          reading: 'すぎ',
          text: 'すぎ',
          seq: 3,
          suffix: 'to be too (much) ...',
          conj: []
        }
      ]
    }))).toBe([
      '食べすぎ 【たべすぎ】 Compound word: 食べ + すぎ',
      ' * 食べる 【たべる】',
      '1. [v1] to eat',
      ' * すぎ  [suffix]: to be too (much) ... '
    ].join('\n'));
  });

  test('numbers alternative readings exactly like word-info-str', () => {
    expect(formatLegacyWordInfo(word({
      alternative: [
        {
          reading: '今日 【きょう】',
          text: '今日',
          seq: 4,
          gloss: [{ pos: '[n,adv]', gloss: 'today' }],
          conj: []
        },
        {
          reading: '今日 【こんにち】',
          text: '今日',
          seq: 5,
          gloss: [{ pos: '[n]', gloss: 'this day' }],
          conj: []
        }
      ]
    }))).toBe([
      '<1>. 今日 【きょう】',
      '1. [n,adv] today',
      '<2>. 今日 【こんにち】',
      '1. [n] this day'
    ].join('\n'));
  });

  test('formats counters and suffixes through their legacy branches', () => {
    const counter = wordInfo(word({
      reading: '三人 【さんにん】',
      text: '三人',
      seq: 6,
      counter: { value: '3 people', ordinal: [] },
      gloss: [{ pos: '[ctr]', gloss: 'counter for people' }]
    }), {
      definitionSeq: 6,
      conjugationSelection: 'default',
      inflected: false
    });
    expect(formatLegacyWordInfo(counter, {
      senses: new Map([[6, [
        { pos: '[ctr]', gloss: 'counter for people' },
        { pos: '[n]', gloss: 'an individual' }
      ]]])
    })).toBe([
      '三人 【さんにん】',
      '3 people',
      '1. [ctr] counter for people',
      '2. [n] an individual'
    ].join('\n'));

    expect(formatLegacyWordInfo(word({
      reading: 'さ',
      text: 'さ',
      seq: 7,
      suffix: '-ness (degree or condition of adjective)',
      conj: []
    }))).toBe('さ  [suffix]: -ness (degree or condition of adjective) ');
  });

  test('formats direct and via conjugation trees in legacy bracket order', () => {
    const via = conjugationInfo({
      prop: [{ pos: 'v1', type: 'Causative-Passive' }],
      reading: '食べる 【たべる】',
      gloss: [{ pos: '[v1,vt]', gloss: 'to eat; to consume' }],
      readok: true
    }, {
      flags: [{ negative: false, formal: false }],
      shortGloss: ''
    });
    const outer = conjugationInfo({
      prop: [{ pos: 'v1', type: 'Past (~ta)', fml: true }],
      via: [via],
      readok: true
    }, {
      flags: [{ negative: false, formal: true }]
    });
    const conjugated = wordInfo(word({
      reading: '食べさせられました',
      text: '食べさせられました',
      seq: 8,
      conj: [outer]
    }), {
      definitionSeq: 8,
      conjugationSelection: 'default',
      inflected: true
    });
    expect(formatLegacyWordInfo(conjugated, { senses: new Map() })).toBe([
      '食べさせられました',
      '',
      '[ Conjugation: [v1] Past (~ta) Affirmative Formal',
      ' --(via)--',
      '[ Conjugation: [v1] Causative-Passive Affirmative Plain',
      '  食べる 【たべる】 :  ] ]'
    ].join('\n'));
  });
});

test('romanizeWithInfo uses top paths and preserves historical reverse definition order', async () => {
  const runtime = {
    romanize: async () => 'kyō wa tenki',
    legacy: async () => [
      [[[
        ['kyō', word({
          reading: '今日 【きょう】',
          seq: 9,
          gloss: [{ pos: '[n]', gloss: 'today' }],
          conj: []
        }), []],
        ['wa', word({
          reading: 'は',
          seq: 10,
          gloss: [{ pos: '[prt]', gloss: 'topic marker' }],
          conj: []
        }), []]
      ], 1]],
      ' ',
      [[[['tenki', word({
        reading: '天気 【てんき】',
        seq: 11,
        gloss: [{ pos: '[n]', gloss: 'weather' }],
        conj: []
      }), []]], 1]]
    ]
  } as unknown as IchiranRuntime;

  expect(await romanizeWithInfo(runtime, '今日は天気')).toEqual({
    romanized: 'kyō wa tenki',
    info: [
      ['tenki', '天気 【てんき】\n1. [n] weather'],
      ['wa', 'は\n1. [prt] topic marker'],
      ['kyō', '今日 【きょう】\n1. [n] today']
    ]
  });
});

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;

describe.skipIf(!releaseDirectory)('real packed legacy -i parity regressions', () => {
  test('preserves unfiltered counter senses', async () => {
    const runtime = await openNodeRuntime(releaseDirectory!);
    expect(await romanizeWithInfo(runtime, '三個')).toEqual({
      romanized: 'sanko',
      info: [[
        'sanko',
        [
          '三個 【さんこ】',
          'Value: 3',
          '1. [ctr] 《also written as ヶ》 counter for (small) things or pieces',
          '2. [ctr] counter for military units',
          '3. [n] (an) individual; one person'
        ].join('\n')
      ]]
    });
  });

  test('preserves affirmative/plain flags and nested via layout', async () => {
    const runtime = await openNodeRuntime(releaseDirectory!);
    expect(await romanizeWithInfo(runtime, '食べさせられました')).toEqual({
      romanized: 'tabesaseraremashita',
      info: [[
        'tabesaseraremashita',
        [
          '食べさせられました 【たべさせられました】',
          '',
          '[ Conjugation: [v1] Past (~ta) Affirmative Formal',
          ' --(via)--',
          '[ Conjugation: [v1] Causative-Passive Affirmative Plain',
          '  食べる 【たべる】 :  ] ]'
        ].join('\n')
      ]]
    });
  });

  test('uses the combined alternative romanization and formats every alternative', async () => {
    const runtime = await openNodeRuntime(releaseDirectory!);
    expect(await romanizeWithInfo(runtime, '行った')).toEqual({
      romanized: 'okonatta/itta',
      info: [[
        'okonatta/itta',
        [
          '<1>. 行った 【おこなった】',
          '',
          '[ Conjugation: [v5u] Past (~ta) Affirmative Plain',
          '  行う 【おこなう】 :  ]',
          '<2>. 行った 【いった】',
          '',
          '[ Conjugation: [v5k-s] Past (~ta) Affirmative Plain',
          '  行く 【いく】 :  ]'
        ].join('\n')
      ]]
    });
  });
});
