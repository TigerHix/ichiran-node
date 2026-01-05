import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いつの間にか.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the いつの間にか grammar rule
const negatives = [
  // いつ (itsu) - "when" (question word alone)
  'いつ東京に行きますか。',
  'いつ誕生日ですか。',

  // 間に (ma ni) - "during, while" (temporal, not unnoticed)
  '勉強している間に電話があった。',
  '寝ている間に雨が降りました。',

  // いつでも (itsu demo) - "anytime" (different compound)
  'いつでも電話してください。',
  'いつでも来てください。',

  // いつまでも (itsu made mo) - "forever" (different compound)
  'いつまでも友達だ。',
  'いつまでも忘れない。',

  // いかにも (ika ni mo) - "indeed, really" (similar ending but different meaning)
  'いかにもおっしゃる通りです。',
  'それはいかにも彼らしい。',

  // Similar adverbs of time but different meaning
  // たちまち (tachimachi) - "immediately, instantly"
  '雨がたちまち降り出した。',
  '私たちはたちまち仲良くなった。',

  // あっという間に (atto iu ma ni) - "in a flash"
  'あっという間に時間が過ぎる。',
  'あっという間に食べ終わった。',

  // 突然 (totsuzen) - "suddenly, abruptly"
  '突然雨が降ってきた。',
  '突然ドアが開いた。',

  // いきなり (ikinari) - "suddenly, without warning"
  'いきなり泣き出した。',
  'いきなり走り出した。',

  // ただちに (tadachini) - "immediately" (formal, with urgency)
  'ただちに行動を開始する。',
  'ただちに対応いたします。',

  // Noun phrases with 間 but not the adverb
  'この間（あいだ）は忙しかった。',
  'あの間（ま）に行ってください。',

  // か as question particle (not part of compound)
  'いつ行くか分からない。',
  '何を食べか決めていない。',

  // いつの間 (itsu no ma) - without final か
  // This is just "what time/interval" not the adverbial phrase
  // (rare but grammatically different)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
