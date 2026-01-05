import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てでも.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the てでも grammar rule
const negatives = [
  // て alone (te-form) - without でも
  '毎日歩いて学校に行きます。',
  '本を読んで勉強しています。',
  'ご飯を食べてください。',
  '窓を開けて涼しくなりました。',
  '彼と話してわかりました。',

  // でも at sentence beginning - "but/however"
  'でも、私は行きたいです。',
  'でも、それは違います。',
  'でも、時間がありません。',

  // で + も (instrumental de + emphasis particle mo)
  '日本語で話します。',
  '鉛筆で書きます。',
  '電車で行きます。',
  'バスでも来ます。',

  // Question word + でも (anything/anyone)
  '何でも食べます。',
  'だれでも来れます。',
  'どこでも行けます。',
  'いつでもいいです。',

  // ても/でも (even if - conditional form, not emphatic てでも)
  '雨が降っても行きます。',
  '高くても買います。',
  '忙しくても勉強します。',

  // てあげる (do something for someone)
  '本を貸してあげました。',
  '手伝ってあげます。',

  // てしまう (regrettable completion)
  '忘れてしまいました。',
  '食べてしまった。',

  // てくる (come and do, or change of state)
  '買ってきます。',
  '暑くなってきました。',

  // てみる (try doing something)
  '食べてみます。',
  'やってみます。',

  // てある (state of being done)
  '書いてあります。',
  '開けてあります。',

  // ている (progressive or state)
  '食べています。',
  '住んでいます。',

  // ておく (do something in advance)
  '予約しておきます。',
  '作っておきます。',

  // てやる (do something for someone inferior)
  '教えてやる。',

  // Noun + でも (even the noun, but not te-form verb)
  '子供でもわかります。',
  '先生でも間違えます。',
  '雨でも行きます。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
