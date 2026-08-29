import { describe, expect, test } from 'bun:test';
import * as current from '../../core/src/dict/numbers.js';
import * as portable from '../src/numbers.js';

const NUMBERS = [
  0, 1, 4, 8, 10, 11, 14, 20, 99, 100, 101, 300, 600, 800,
  1000, 3000, 8000, 10_000, 12_345, 100_000_001, 9_876_543_210,
  100_000_000_000_000
] as const;

describe('portable Japanese numbers', () => {
  test('matches kanji and kana generation', () => {
    for (const number of NUMBERS) {
      expect(portable.numberToKanji(number)).toBe(current.numberToKanji(number));
      expect(portable.numberToKana(number)).toEqual(current.numberToKana(number));
      expect(portable.numberToKana(number, { separator: null })).toEqual(
        current.numberToKana(number, { separator: null })
      );
    }
  });

  test('matches parsing over generated and written forms', () => {
    for (const number of NUMBERS) {
      const kanji = current.numberToKanji(number);
      expect(portable.parseNumber(kanji)).toBe(current.parseNumber(kanji));
    }
    for (const input of ['０', '123', '一二三', '千二百三十四', '壱拾参', '一億二万三']) {
      expect(portable.parseNumber(input)).toBe(current.parseNumber(input));
    }
  });

  test('rejects invalid input with the same message', () => {
    for (const input of ['abc', '一a', 'かな']) {
      expect(() => portable.parseNumber(input)).toThrow(portable.NotANumberError);
      try {
        portable.parseNumber(input);
      } catch (portableError) {
        try {
          current.parseNumber(input);
        } catch (currentError) {
          expect((portableError as Error).message).toBe((currentError as Error).message);
        }
      }
    }
  });
});
