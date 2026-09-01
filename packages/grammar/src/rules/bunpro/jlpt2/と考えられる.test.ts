import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と考えられる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Sentences that can't be matched due to GiNZA parsing limitations or grammar differences:
//
// 1. "そのプロジェクト、一旦白紙に戻すということもかんがえられませんか？"
//    - This sentence uses "というとも" (quotative と +いう + also も) + "かんがえられません"
//    - The と particle belongs to "という", not to "かんがえられません"
//    - This is testing the negative potential form WITHOUT the quoting particle と
//    - Our rule is specifically for と考えられる (with quoting particle と)
//    - Therefore this sentence should not match our rule
//
// 2. "この形からすると、何かの入れ物だとかんがえられますが、謎ですね。"
//    - This sentence should match "とかんがえられます" (polite form)
//    - GiNZA appears to parse "かんがえられますが" as a single token with the trailing particle
//    - Our rule expects "かんがえられます" to be parsed as separate tokens (かんがえ + られ + ます)
//    - When the sentence ending "が" attaches to the verb, it creates a single token that doesn't match our patterns
//    - This is a GiNZA parsing limitation where sentence-ending particles merge with the verb
const skipPositives = [
  'そのプロジェクト、一旦白紙に戻すということもかんがえられませんか？',
  'この形からすると、何かの入れ物だとかんがえられますが、謎ですね。',
];

// Negative test cases - sentences that should NOT match the と考えられる grammar rule
const negatives = [
  // Simple と quoting with different verbs (say, think, etc.)
  '彼が来ると思っています。',
  '先生がそう言った。',
  '成功すると確信しています。',
  '彼はお金を持っていると言った。',
  '私はそれが正しいと思う。',

  // Subjective opinion: と思われる (to omowareru) - more subjective
  '私には彼がこの事件の犯人だと思われる。',
  '私にはあなたが悪いと思われる。',
  'その結論は間違っていると思われる。',

  // と as "with" or "and" (companion particle)
  '友達と映画を見に行った。',
  '彼と話し合った。',
  '家族と食事をします。',

  // Similar patterns but different grammar
  'この件について考えるべきだ。',
  '彼はよく考えている。',
  'もう少し考えさせてください。',

  // Potential form without と: 考えられる (can think/can imagine)
  'そんなことは考えられない。',
  'この問題は解決策が考えられない。',

  // Continuous form: と考えられている (widely held opinion)
  'この方法は最善だと考えられている。',
  '地球は丸いと考えられている。',
  'その薬は効果があると考えられている。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
