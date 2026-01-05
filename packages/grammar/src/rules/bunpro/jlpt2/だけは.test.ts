import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけは.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the だけは grammar rule
const negatives = [
  // Simple だけ (only/just) without topic marker は
  // These are just restrictive "only" statements, not "at least" emphasis
  'これだけは食べてください。',
  '今日だけは早く帰りたい。',
  'お金だけあれば幸せだ。',

  // だけで (only by/with) - different particle
  'これだけで十分です。',
  'それだけでいいんだ。',

  // だけに (precisely because) - different nuance
  '高いだけに品質がいい。',
  '子供だけに大切にしたい。',

  // Noun + だけ (noun only, not verb repetition)
  'これだけを見て。',
  'あれだけが好きだ。',

  // Verb + だけ (verb only, not followed by same verb)
  '食べるだけ食べた。',
  '行くだけ行くけど。',

  // は as topic marker on other structures (not だけ+は pattern)
  'これは本です。',
  '彼は学生だ。',

  // だけ + は but without verb repetition (different grammar)
  // These might be legitimate sentence structures but not the だけは pattern
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
