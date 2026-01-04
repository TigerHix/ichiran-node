import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とされている.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases - sentences that should NOT match
const negatives = [
  // Direct quotation with する in active voice (not passive)
  // "彼は「必要」だとする。" - He regards it as necessary.
  // This uses active する, not passive されている

  // Different grammar: といわれている (it is said that)
  // "最高といわれている" - It is said to be the best

  // Different grammar: とかんがえられている (is thought of as)
  // "一番だとかんがえられている" - It is thought of as the best

  // Different grammar: ということだ (it means that)
  // "来週だということだ" - It means it's next week

  // Different grammar: として (as)
  // "学生として" - As a student

  // Different grammar: とした (decided on/assumed)
  // "必要とした" - Decided it was necessary

  // する in active voice
  // "彼はそうする。" - He does so.

  // せる (causative) - different auxiliary
  // "行かせる。" - Make someone go.

  // Just とすれば (if we assume)
  // "东京とすれば..." - Assuming it's Tokyo...

  // としても (even as / even if)
  // "遊びとしても..." - Even as a play...
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
