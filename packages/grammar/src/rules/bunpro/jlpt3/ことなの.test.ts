import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことなの.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences that should NOT match this pattern
const negatives = [
  // Simple こと + だ (without なの explanation)
  'これは大切なことだ。',
  '彼が来ることは確かだ。',
  // こと + が (different particle)
  '彼のことが好きだ。',
  'そんなことがあってはならない。',
  // こと + に + な (different grammar - koto ni naru)
  '夢がことになる。',
  // ということ (without なの - different grammar)
  'これは重要なことだということを知っておいてください。',
  // なの without こと (different explanatory pattern)
  '私は学生なのです。',
  'これは私の本なの。',
  // なん without こと (different pattern)
  'なんだこれ。',
  'なんと美しい花だろう。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
