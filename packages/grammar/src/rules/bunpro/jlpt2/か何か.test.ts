import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './か何か.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the か何か grammar rule
const negatives = [
  // かどうか (ka dou ka) - "whether or not" (different grammar)
  '面白いかどうか分からない。',
  '行くかどうかまだ決めてない。',
  '本当かどうか確認してください。',
  '彼が来るかどうかわからない。',

  // かも (ka mo) - "might be" (possibility marker)
  '雨が降るかも。',
  '明日は晴れるかも。',
  '彼は忙しいかも。',
  '駅は近いかも。',

  // かというと (ka to iu to) - "as for, speaking of"
  '彼が遅れたかというと、電車が遅れたからだ。',
  '成功したかというと、そうとも言えない。',

  // か + noun (question marker + noun, not "or something")
  'これか？あれか？',
  'リンゴかバナナか選んで。',
  '行くか行かないか、早く決めて。',

  // 何か alone (something, but not "or something")
  '何か食べたい。',
  '何かあったの？',
  '何か面白いことない？',

  // でも (demo) - "or something, but, however"
  'お茶でも飲みませんか？',
  'コーヒーでもどうですか。',
  '公園でも行こう。',

  // とか (toka) - "and things like, or something like"
  'りんごとかバナナとか買った。',
  '映画とか見たい。',
  '東京とか大阪に行きたい。',

  // か (question particle alone)
  '明日来るか？',
  'これ美味しいか？',
  '彼は忙しいか？',

  // 何 + noun (what + noun, not "or something")
  '何色が好き？',
  '何時に行く？',
  '何日かかる？',

  // かな (kana) - "I wonder"
  '明日は雨かな。',
  '彼は来るかな。',

  // かねる (kaneru) - "hard to do"
  '賛成しかねる。',
  '決めかねる。',

  // かねない (kanenai) - "might well"
  '火事になりかねない。',
  '失敗しかねない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
