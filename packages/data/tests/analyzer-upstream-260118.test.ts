import { expect, test } from 'bun:test';
import type { KanjiText, KanaText } from '../../reference-postgres/src/types.js';
import {
  UPSTREAM_260118_EASY_HINTS,
  UPSTREAM_260118_GATAI_CLASS,
  UPSTREAM_260118_GATAI_KEYWORD,
  UPSTREAM_260118_GATAI_SEQ,
  UPSTREAM_260118_NEBA_ABBREVIATION,
  UPSTREAM_260118_SKIP_WORD_ADDED,
  UPSTREAM_260118_SKIP_WORD_REMOVED,
  upstream260118HintMap,
  upstream260118SplitMap
} from '../src/browser-pack/analyzer-upstream-260118.js';

test('pins the ea958336 compiler-owned analyzer overlay inventory', () => {
  expect([...upstream260118SplitMap.keys()]).toEqual([1_774_820, 1_362_970]);
  expect([...upstream260118HintMap.keys()]).toEqual([
    2_867_144,
    2_867_149,
    ...UPSTREAM_260118_EASY_HINTS.map(([seq]) => seq)
  ]);
  expect(UPSTREAM_260118_EASY_HINTS).toHaveLength(11);
  expect({
    seq: UPSTREAM_260118_GATAI_SEQ,
    keyword: UPSTREAM_260118_GATAI_KEYWORD,
    className: UPSTREAM_260118_GATAI_CLASS,
    abbreviation: UPSTREAM_260118_NEBA_ABBREVIATION,
    skipAdded: UPSTREAM_260118_SKIP_WORD_ADDED,
    skipRemoved: UPSTREAM_260118_SKIP_WORD_REMOVED
  }).toEqual({
    seq: 2_867_504,
    keyword: ':ren-',
    className: ':gatai',
    abbreviation: { text: 'ねば', keyword: ':nakereba' },
    skipAdded: 2_827_357,
    skipRemoved: 2_458_040
  });
});

test('keeps the new からすき split kana-only and inserts Go-sentence は hints', async () => {
  const kanji: KanjiText = {
    id: 1,
    seq: 1_774_820,
    text: '来るからすき',
    ord: 0,
    common: null,
    commonTags: '',
    conjugateP: false,
    nokanji: false,
    bestKana: 'くるからすき'
  };
  expect(await upstream260118SplitMap.get(1_774_820)!(kanji)).toBeNull();

  const kana: KanaText = {
    id: 2,
    seq: 2_867_144,
    text: 'ふためのあたまはみずはねよ',
    ord: 0,
    common: null,
    commonTags: '',
    conjugateP: false,
    nokanji: true,
    bestKanji: null,
    hintedp: true
  };
  expect(await upstream260118HintMap.get(2_867_144)!(kana)).toBe(
    'ふためのあたま\u200b\u200cは\u200bみずはねよ'
  );
});
