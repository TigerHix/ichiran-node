import { describe, expect, test } from 'bun:test';
import * as current from '../../reference-postgres/src/romanize.js';
import {
  processHints as currentProcessHints,
  stripHints as currentStripHints
} from '../../reference-postgres/src/dict/splitDefinitions.js';
import { ALL_CHARACTERS } from '../src/characters.js';
import {
  joinRomanizedParts,
  processHints,
  romanizationMethods,
  romanizeWord,
  stripHints,
  type RomanizationName
} from '../src/romanization.js';

const METHODS: readonly [RomanizationName, current.RomanizationMethod][] = [
  ['hepburn-basic', current.hepburnBasic],
  ['hepburn-simple', current.hepburnSimple],
  ['hepburn-passport', current.hepburnPassport],
  ['hepburn-traditional', current.hepburnTraditional],
  ['hepburn-modified', current.hepburnModified],
  ['kunrei-siki', current.kunreiSiki]
];

const KANA = [...new Set(Object.values(ALL_CHARACTERS).join(''))];
const WORDS = [
  ...KANA,
  'きゃ', 'しゃ', 'ちゃ', 'じゃ', 'ぢゃ', 'ふぁ', 'うぉ',
  'がっこう', 'まっちゃ', 'しんぶん', 'しんよう', 'スーパー',
  'おおさか', 'とうきょう', 'おねえさん', 'ヴァイオリン',
  '時々', 'いすゞ', 'は\u200cは', 'へ\u200cへ', 'aかな12'
];

describe('portable romanization', () => {
  test('matches every current method over kana and edge cases', () => {
    for (const [name, currentMethod] of METHODS) {
      expect(romanizationMethods[name]).toBeDefined();
      for (const word of WORDS) {
        expect(romanizeWord(word, { method: name })).toBe(
          current.romanizeWord(word, { method: currentMethod })
        );
      }
    }
  });

  test('matches normalization and original-spelling special cases', () => {
    for (const word of ['ﾄｳｷｮｳ', 'ｳﾞｧ', 'す゛し', 'っ', 'ー']) {
      expect(romanizeWord(word)).toBe(current.romanizeWord(word));
      expect(romanizeWord(word, { normalize: false })).toBe(
        current.romanizeWord(word, { normalize: false })
      );
    }
    expect(romanizeWord('かな', { originalSpelling: 'っ' })).toBe('!');
    expect(romanizeWord('かな', { originalSpelling: 'ー' })).toBe('~');
  });

  test('matches hint processing and part joining', () => {
    const hints = ['は\u200cは', 'へ\u200cへ', 'a\u200bかな', '\u200cハ\u200cヘ'];
    for (const input of hints) {
      expect(processHints(input)).toBe(currentProcessHints(input));
      expect(stripHints(input)).toBe(currentStripHints(input));
    }

    const parts = ['Tōkyō', 'to', ', ', 'Ōsaka', '', '2026'];
    expect(joinRomanizedParts(parts)).toBe(current.joinParts(parts));
  });
});
