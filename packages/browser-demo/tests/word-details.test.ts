import { describe, expect, test } from 'bun:test';
import type { DictionaryEntry } from '../src/analyzer-service.js';
import { presentedSenses } from '../src/WordDetails.js';

const entry: DictionaryEntry = {
  seq: 1,
  forms: [],
  senses: [
    {
      ord: 1,
      glosses: [{ ord: 1, text: 'first meaning' }],
      properties: [
        { tag: 'pos', ord: 1, text: 'n' },
        { tag: 'misc', ord: 2, text: 'uk' },
        { tag: 's_inf', ord: 3, text: 'usage note' }
      ]
    },
    {
      ord: 2,
      glosses: [{ ord: 1, text: 'carried noun meaning' }],
      properties: []
    },
    {
      ord: 3,
      glosses: [{ ord: 1, text: 'restricted meaning' }],
      properties: [
        { tag: 'pos', ord: 1, text: 'vi' },
        { tag: 'stagk', ord: 2, text: '別の形' }
      ]
    },
    {
      ord: 4,
      glosses: [{ ord: 1, text: 'carried verb meaning' }],
      properties: []
    }
  ]
};

describe('word detail sense presentation', () => {
  test('carries POS per JMdict rules and filters form restrictions', () => {
    const senses = presentedSenses(entry, 'kanji', '見出し', 'みだし', []);
    expect(senses.map(sense => ({ gloss: sense.gloss, pos: sense.pos }))).toEqual([
      { gloss: 'first meaning', pos: ['n'] },
      { gloss: 'carried noun meaning', pos: ['n'] },
      { gloss: 'carried verb meaning', pos: ['vi'] }
    ]);
  });

  test('shows prose usage information without dumping internal metadata codes', () => {
    const senses = presentedSenses(entry, 'kanji', '見出し', 'みだし', []);
    expect(senses[0]?.info).toBe('usage note');
    expect(JSON.stringify(senses)).not.toContain('uk');
  });
});
