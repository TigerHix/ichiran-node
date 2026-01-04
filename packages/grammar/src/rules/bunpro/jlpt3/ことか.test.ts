import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことか.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: similar-looking patterns that should NOT match
const negatives = [
  // Simple question with か (not the ことか exclamation pattern)
  'することができますか。',
  'これは何ですか。',
  // こと as regular noun + か (question about a thing/matter)
  'そんなことがありますか。',
  'このことを知っていますか。',
  // ことだ + か (copula + question)
  'それは重要なことですか。',
  '一番大切なことは何か。',
  // ことが + か (different particle)
  '彼のことが好きか。てめぇは誰だ',
  // Regular noun + こと + か (not nominalized)
  '何かということがある。',
  // Note: "何かことか。" and "何をことかと思った。" are actually valid ことか patterns
  // (exclamations about "what a thing!"), so they are NOT included in negatives
  // こと inside a different grammatical construction
  'ことができるかどうか。',
  'ことだから心配だ。',
  // こと + で + か (different structure)
  'ことでか決める。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
