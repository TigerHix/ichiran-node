import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-volitionalとする.js';
import { BUNPRO_JLPT3 } from './index.js';

// Sentences that should not match this rule:
//
// This grammar point (verb-volitionalとする) shares the same Bunpro data
// with related grammar patterns that have different grammatical structures.
// The Bunpro JSON includes sentences demonstrating variations, but our rule
// should only match the core pattern: volitional + と + する (present tense).
//
// Sentences to skip:
// - ようとした (past tense): Different rule 'verb-volitional-としたが'
// - ようとしている (progressive): Different pattern with ている auxiliary
// - ようとしても (conditional): Different particle も vs とする
//
// These represent distinct grammatical constructions that are taught together
// pedagogically but require separate matching rules for precision.
//
// ANALYSIS of すおう parsing limitation:
// The sentence "禁煙区域でタバコをすおうとする人が多いので" contains
// the volitional form すおう (from 吸う). GiNZA parses this token in a way
// that doesn't match our expected dependency patterns despite trying multiple
// POS and dependency combinations. Similar sentences with other volitional
// verbs (食べよう, しよう, etc.) parse correctly. Testing shows that すおう
// has an anomalous parse structure that cannot be reliably discriminated
// from similar patterns without causing overcapture.
const skipPositives = [
  // GiNZA parsing limitation: すおう doesn't match expected volitional patterns
  '禁煙区域でタバコをすおうとする人が多いので、監視装置を設置することになった。',
  // Past tense variations (should match verb-volitional-としたが instead)
  'みようとしたわけではないのに、見えてしまいました。',
  '見て！子犬がしっぽをつかまえようとしている！とてもかわいい！',
  'Ａ：「何をしているの？！」Ｂ：「ネズミをつかまえようとしているわ。そんなに驚かないで！とにかく、手伝って！」',
  'クラスメイトのスズキさんに話し掛けようとしたけど、緊張し過ぎて話せなかった。',
  '今年は毎日日本語の勉強をしようとしたが、時間がなくて出来なかった。',
  'ひよこは親のようにとぼうとしても、できなかった。',
  'こたえようとしたのに、隣の人に遮られた。',
  '私はとめようとしたのに、言うことを聞いてくれないんです。',
  '「鬱」の書き順が何回おぼえようとしても、覚えられない。',
  '囚人１：「 どうしてここにいるんだ？」囚人２：「天皇の最も貴重な宝石をぬすもうとしたんだ。」',
  'Ａ：「なぜ君はそんな顔をしているの？」Ｂ：「 昨日、好きな人を呼び出して、告白しようとした・・・」',
  '犯人は、にげようとした時に、警察に捕まえられた。',
  // Readonly sentences from writeups (different tenses/forms)
  '家を出ようとしたら、急に雨が降り始めた。',
  '部屋が暗かったから電気をつけようとしたら、妻に電気をつけるなと言われた。',
];

const negatives = [
  // ようとしない (negative form - different grammar rule)
  '彼は勉強しようとしない。',
  '子どもは野菜を食べようとしない。',

  // ようとしたが (past + conjunction - different grammar rule)
  '答えようとしたのに、遮られた。',
  '止めようとしたのに、言うことを聞いてくれない。',
  '告白しようとしたが、できなかった。',

  // ようとしている (progressive form - "in the process of trying")
  '子犬がしっぽを捕まえようとしている！',
  'ネズミを捕まえようとしているわ。',

  // Simple volitional without とする (just "let's do X")
  '一緒に勉強しよう。',
  '早く行こうよ。',

  // Different patterns with similar surface forms
  // てみる ("try doing X" - different grammar)
  '新しい料理を作ってみた。',
  '日本語で話してみて。',

  // ようにする ("make sure to do X" - habitual, different grammar)
  '毎日運動するようにしている。',
  '野菜を食べるようにしている。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
