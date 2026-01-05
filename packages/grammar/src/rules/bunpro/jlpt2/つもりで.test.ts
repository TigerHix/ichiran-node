import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './つもりで.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the つもりで grammar rule
const negatives = [
  // つもりだ (sentence-final intention, different grammar)
  '来月日本に行くつもりだ。',
  '私は会社を辞めるつもりだ。',
  '明日は早く起きるつもりです。',
  '彼はその本を読むつもりがない。',
  'やるつもりはなかったんですが、結局やることになりました。',

  // Simple instrumental で (with, by, using) - different usage
  '日本語で勉強しています。',
  '電車で会社へ行きます。',
  '鉛筆で書きました。',
  'パソコンで作業しています。',
  '車で移動します。',

  // つもり without で (incomplete pattern)
  '行くつもりだが、雨が降ったから行かなかった。',
  '彼は来るつもりがあるらしい。',
  'そんなつもりはない。',

  // Location で (at, in) - different particle usage
  '図書館で勉強しました。',
  '公園で遊びます。',
  '家でテレビを見る。',

  // Cause/reason で (because of, due to)
  '病気で学校を休みました。',
  '雨で試合が中止になった。',
  '事故で電車が遅れている。',

  // つもり in different contexts (not the grammar pattern)
  '私のつもりでは、これは正しいやり方だ。',
  '彼女のつもりは分からない。',
  'そんなつもりで言ったんじゃない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
