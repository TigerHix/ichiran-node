import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './というわけではない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the というわけではない grammar rule
const negatives = [
  // わけではない (without という) - simpler negation pattern
  'できないわけではない。',
  '行きたくないわけではない。',
  '嫌いなわけではない。',

  // という (quotative) + other patterns (not wake dewa nai)
  'これは何という花ですか。',
  '東京という都市は大きい。',
  '彼は行くと言った。',
  'これは何と言う意味ですか。',

  // ということだ (positive assertion - "that means")
  '彼が来るということだ。',
  'つまり、嘘だということです。',

  // というものではない (different nuance - "not necessarily")
  '努力すれば必ず成功するというものではない。',
  'お金さえあれば幸せというものではない。',

  // からといって (concessive - "just because... doesn't mean")
  '安いからといって買いすぎてしまった。',
  '日本人だからといって、漢字を書けるとは限らない。',

  // Simple negation with ではない (not wake dewa nai pattern)
  '彼は学生ではない。',
  'これは私の本ではない。',
  '今日は休日ではない。',

  // Similar but unrelated patterns
  // という + わけ + だ (positive form)
  '以前この国で犯罪を犯したからというわけだ。',
  '彼を押し倒したという訳だ。',

  // という + わけ + には + いかない (must not)
  // (Different grammar pattern)

  // という + わけ + が (contrastive conjunction)
  // (Could be followed by が instead of ではない)

  // では alone (locative or topic marker)
  '東京では電車が便利です。',
  '日本では桜が有名です。',

  // じゃ alone (casual topic marker or copula)
  'これじゃだめだ。',
  '今日は休みじゃない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
