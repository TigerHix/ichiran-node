import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てならない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the てならない grammar rule
const negatives = [
  // てしょうがない (te shouganai) - similar but more colloquial
  '会いたくてしょうがない。',
  '暑くてしょうがない。',
  '心配でしようがない。',

  // てたまらない (tamaranai) - unbearable intensity (not for thinking/feeling verbs)
  '会いたくてたまらない。',
  '暑くてたまらない。',
  'お腹が空いてたまらない。',

  // Simple て-form verbs without ならない
  '本を読んで勉強しました。',
  '朝ごはんを食べて学校に行きます。',
  '友達と映画を見て楽しかった。',

  // て alone (te-form without auxiliary)
  '宿題が終わって、遊びに行きました。',
  '雨が降って、凉しくなりました。',

  // て + other auxiliaries
  '本を読んでいる。',
  '雨が降っています。',
  '彼は来てしまった。',

  // Na-adj + だ (copula) + other expressions
  '彼は学生だそうだ。',
  'これは便利だと思う。',

  // で (de) as instrumental/locative particle
  '東京で働きます。',
  '鉛筆で書く。',
  '日本語で話す。',

  // くて (kute) + other endings
  'この部屋は広くて明るい。',
  '彼は背が高くてハンサムだ。',

  // なら (nara) - conditional form of だ/なる
  '雨なら行きません。',
  'できるならやってみて。',

  // ない (nai) - simple negation
  '私は行かない。',
  '彼は来ない。',

  // Negative verb forms with て but without ならない
  '行かなくてはいけない。',
  '食べなくていい。',

  // てから (tekara) - after doing
  'ご飯を食べてから出かけます。',
  '結婚してから名字が変わった。',

  // Other grammar using te-forms
  '勉強してみました。',
  '行ってみたいです。',
  '読んでおきます。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
