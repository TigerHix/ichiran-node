import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './といわれている.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases - sentences that should NOT match
const negatives = [
  // Direct quotation with 言う in active voice (not passive)
  // "彼は「危ない」と言った。" - He said "It's dangerous."
  // This uses active 言った, not passive 言われている

  // Plain quotation particle と without 言われている
  // "彼は行くと言った。" - He said he would go.

  // Different grammar: という (called/named)
  // "ポケモンというゲーム" - A game called Pokemon

  // Different grammar: ということだ (it means that)
  // "来週だということだ" - It means it's next week

  // Different grammar: とされている (it is considered that)
  // "最高とされている" - It is considered the best

  // 言う in active voice
  // "彼はそう言った。" - He said so.

  // 言える (can say) - different verb
  // "何とも言えない。" - Cannot say anything (can't tell).

  // 言わず (without saying) - different conjugation
  // "言わずに済んだ。" - Settled without saying.

  // Just といえば (speaking of)
  // "東京といえば..." - Speaking of Tokyo...

  // と言っても (even if you say)
  // "安いと言っても..." - Even if you say it's cheap...
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
