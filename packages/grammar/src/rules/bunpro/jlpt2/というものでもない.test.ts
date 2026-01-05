import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './というものでもない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the というものでもない grammar rule
const negatives = [
  // という (to iu) alone - "called/known as" (without ものでもない)
  'これは何という花ですか。',
  '東京という都市は大きい。',
  '彼は田中という人です。',
  'これは何という意味ですか。',

  // もの (mono) alone - "thing" (in different contexts)
  'いいものを買いました。',
  'これは大切なものです。',
  '彼はものをよく知っている。',
  'ものの考え方が違う。',

  // でも (demo) alone - "but/even" (conjunction)
  '行きたいけど、でも時間がない。',
  '彼は英語でも話せる。',
  '子供でもできる。',

  // というわけではない (to iu wake dewa nai) - different grammar pattern
  // This is similar but uses わけ instead of もの
  '彼が嫌いというわけではない。',
  'できないというわけではない。',

  // わけではない (wake dewa nai) - "not necessarily the case" (different nuance)
  'できないわけではない。',
  '行きたくないわけではない。',

  // とは限らない (to wa kagiranai) - "not necessarily" (different structure)
  '高いものが必ずしも良いとは限らない。',
  '日本人だからといって漢字が書けるとは限らない。',

  // ことにはならない (koto ni wa naranai) - "doesn't mean that"
  '勉強したからといって合格することにはならない。',

  // Simple negation with ない (nai) without the full pattern
  'これはいいものではない。',

  // という (quotative) + different noun (not もの)
  '成功という結果を得た。',
  '彼は天才というほどではない。',

  // Similar patterns that lack the complete structure
  // という + noun (but not もの) + でも + ない
  'これは重要という点でもない。',

  // Patterns that look similar but are grammatically different
  'これは買うものではない。',
  'これは食べるものでもない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
