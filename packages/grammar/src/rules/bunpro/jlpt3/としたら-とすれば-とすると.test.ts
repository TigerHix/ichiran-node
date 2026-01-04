import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './としたら-とすれば-とすると.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple と quotation (not hypothetical)
  // "彼は行くと言った" - He said he will go
  // This uses と as a quotation marker, not as hypothetical assumption

  // Conditional たら without とする (different grammar)
  // "行ったら分かる" - When you go, you'll understand

  // として (toshite) meaning "as" or "in the role of" (different grammar)
  // "学生として参加する" - Participating as a student

  // としても (toshitemo) meaning "even if" (different grammar)
  // "行っても行かなくても" - Even if you go or don't go

  // Simple と conditional for real-world cause-and-effect
  // "春になると花が咲く" - When spring comes, flowers bloom
  // This is different from hypothetical としたら

  // にしたら (nishitara) meaning "from the point of view of" (different grammar)
  // "私にしたら、それは難しい" - From my perspective, that's difficult
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
