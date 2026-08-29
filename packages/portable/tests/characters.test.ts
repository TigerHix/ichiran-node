import { describe, expect, test } from 'bun:test';
import * as current from '../../core/src/characters.js';
import * as portable from '../src/characters.js';

const TEXT_CASES = [
  '',
  '食べました。',
  '東京２０２６年８月２８日',
  'ﾊﾟﾝを二つ、ｺｰﾋｰを１杯。',
  '「何で？」と言った・・・',
  'す゛っと',
  '時々々',
  'abc, 12.34 / 日本語',
  '右に出る者はいない',
  'ヴァイオリンとゔぁいおりん'
] as const;

describe('portable character behavior', () => {
  test('matches normalization and basic splitting', () => {
    for (const input of TEXT_CASES) {
      expect(portable.normalize(input)).toBe(current.normalize(input));
      expect(portable.normalize(input, undefined, true)).toBe(
        current.normalize(input, undefined, true)
      );
      expect(portable.normalize(input, 'kana')).toBe(current.normalize(input, 'kana'));
      expect(portable.basicSplit(input)).toEqual(current.basicSplit(input));
    }
  });

  test('matches character conversion and measurement', () => {
    for (const input of TEXT_CASES) {
      expect(portable.asHiragana(input)).toBe(current.asHiragana(input));
      expect(portable.asKatakana(input)).toBe(current.asKatakana(input));
      expect(portable.moraLength(input)).toBe(current.moraLength(input));
      expect(portable.sequentialKanjiPositions(input, 3)).toEqual(
        current.sequentialKanjiPositions(input, 3)
      );
      for (const charClass of [
        'katakana', 'katakana-uniq', 'hiragana', 'kanji', 'kanji-char',
        'kana', 'traditional', 'nonword', 'number'
      ] as const) {
        expect(portable.testWord(input, charClass)).toBe(current.testWord(input, charClass));
        expect(portable.countCharClass(input, charClass)).toBe(
          current.countCharClass(input, charClass)
        );
        expect(portable.collectCharClass(input, charClass)).toEqual(
          current.collectCharClass(input, charClass)
        );
        expect(portable.consecutiveCharGroups(charClass, input)).toEqual(
          current.consecutiveCharGroups(charClass, input)
        );
      }
    }
  });

  test('matches kana mutations and stemming', () => {
    const kana = Object.values(current.KANA_CHARACTERS).join('');
    for (const character of kana) {
      const input = character + 'な';
      expect(portable.unrendaku(input)).toBe(current.unrendaku(input));
      expect(portable.rendaku(input)).toBe(current.rendaku(input));
      expect(portable.rendaku(input, false, true)).toBe(current.rendaku(input, false, true));
      expect(portable.geminate(input)).toBe(current.geminate(input));
    }

    for (const input of TEXT_CASES) {
      for (let stem = 0; stem < 6; stem++) {
        expect(portable.destem(input, stem)).toBe(current.destem(input, stem));
      }
    }
  });
});
