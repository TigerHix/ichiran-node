import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とかんがえられている.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases - sentences that should NOT match
const negatives = [
  // Direct quotation with 考える in active voice (not passive)
  // "彼は「面白い」と考えた。" - He thought "It's interesting."
  // This uses active 考えた, not passive 考えられている

  // Direct quotation with 思う in active voice (not passive)
  // "彼は「そうだ」と思った。" - He thought "That's right."
  // This uses active 思った, not passive 思われている

  // Different grammar: といわれている (it is said that)
  // "危ないと言われている" - It is said that it's dangerous

  // Different grammar: とされている (it is considered that - weaker nuance)
  // "最高とされている" - It is considered the best

  // Different grammar: と考えられる (can be considered / potential)
  // "効果的だと考えられる" - It can be considered effective

  // Different grammar: という (called/named)
  // "ポケモンというゲーム" - A game called Pokemon

  // 考える in active voice
  // "彼はよく考えた。" - He thought carefully.

  // 思う in active voice
  // "私はそう思う。" - I think so.

  // 考えず (without thinking) - different conjugation
  // "考えずに答えた。" - Answered without thinking.

  // Just とすれば (if we consider)
  // "例えばとすれば..." - If we take for example...

  // として (as) - different grammar
  // "学生として" - As a student
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
