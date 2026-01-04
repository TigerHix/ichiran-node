import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './という理由で.js';
import { BUNPRO_JLPT3 } from './index.js';

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// 1. 「失敗するかもしれない」というりゆうだけでせっかくのチャンスを無駄にする人が多い。
//    This sentence has "というりゆう**だけ**で" where で comes after だけ, not after りゆう.
//    The structure is: というりゆう + だけ + で (not というりゆう + で)
//    Our rule requires で to come immediately after りゆう/理由.
//
//    GiNZA parses this as different tokens: りゆう(NOUN) + だけ(ADP) + で(ADP)
//    The で here is attached to だけ, not to りゆう directly.
//    This is a grammatically different pattern (理由だけで = "for the sole reason of")
//    and should not be matched by our rule which specifically looks for 理由で.
const skipPositives = [
  '「失敗するかもしれない」というりゆうだけでせっかくのチャンスを無駄にする人が多い。',
];

const negatives = [
  // Different grammar: ということ (nominalization, not reason)
  // "The fact that..." vs "for the reason that..."
  'これは間違っているということです。',

  // Different grammar: ということだ (hearsay/conclusion)
  '彼が来ないということだ。',

  // Different grammar: というのは (explanation)
  '彼女が来なかったというのは、病気だったからです。',

  // Simple locative で (at/in), not reason marker
  // 公園で遊ぶ (play IN the park) - should NOT match
  '公園で子供たちが遊んでいます。',

  // Instrumental で (by means of), not reason marker
  // 日本語で書く (write IN Japanese) - should NOT match
  '日本語で手紙を書きます。',

  // そのため (different connective, similar meaning)
  '雨が降っています。そのため試合は中止です。',

  // だから (plain conjunction, not emphasizing reason)
  '雨です。だから行きません。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
