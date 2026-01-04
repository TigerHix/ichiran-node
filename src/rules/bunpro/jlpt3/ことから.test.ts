import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことから.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences with こと+から that should NOT match this pattern
const negatives = [
  // Simple noun こと with から meaning "from" (not nominalization)
  'ことから始める。',
  // こと as regular noun + から (origin/source)
  'このことから始まった。',
  // こと as object + から (from/starting with this matter)
  'このことから考え直す。',
  // Different pattern: こと + に + から (not the target structure)
  'ことになるとからだ。',
  // こと + が + から (different particle)
  'ことがから見える。',
  // ことだ + から (copula + because, different grammar)
  'それは重要なことだから、忘れないで。',
  // こと + で + から (different particle combination)
  'ことでから言うと',
  // Simple noun phrase + から (not nominalized clause)
  '彼のことから考える。',
  // こと inside a different grammatical construction
  'ことができるから。',
  'ことだから心配だ。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
