import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いきなり.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the いきなり grammar rule
const negatives = [
  // 行く (iku) - different verb (to go)
  'すぐに行きます。',
  '彼は行きなさいと言った。',

  // 成る (naru) - different verb (to become)
  'そうなるでしょう。',
  '夢が実現した。',

  // Similar adverbs with related meanings but different words
  // 急に (kyuu ni) - suddenly (general suddenness)
  '急に雨が降ってきた。',
  '彼は急に立ち上がった。',

  // 突然 (totuzen) - suddenly, unexpectedly
  '突然の訪問者に驚いた。',
  '突然電話が鳴った。',

  // たちまち (tachimachi) - immediately, instantly
  'たちまち広まった。',
  '彼はたちまち成功した。',

  // ただちに (tadachini) - immediately (with urgency)
  'ただちに行動を開始する。',

  // すぐに (sugu ni) - immediately, soon (neutral)
  'すぐに戻ります。',
  'すぐに行きましょう。',

  // いつの間にか (itsu no ma ni ka) - unnoticed, before realizing
  'いつの間にか夜になっていた。',
  'いつの間にか春が来ていた。',

  // はやく (hayaku) - quickly, early
  'はやく来てください。',
  '朝早く起きた。',

  // やっぱり (yappari) / やはり (yahari) - as expected, after all
  'やっぱり雨が降ってきた。',
  'やはり彼は来なかった。',

  // いき (iki) - different word (vivid, fresh)
  'いき絵を描く。',

  // なり (nari) - different word (conditional, or verb suffix)
  '雨なら行きません。',
  '来るなり寝た。',

  // Phrases that contain "いき" or "なり" but not the compound word
  '生き生きとした表情。',

  // 行き成り as individual characters (not the compound adverb)
  // Unlikely to occur in natural text without being the adverb
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
