import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かな.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that should NOT match (similar patterns but different grammar)
const negatives = [
  // Regular question with か (not かな)
  '明日は雨ですか。',
  '何か買いますか。',
  // な sentence-final particle alone (not かな)
  '難しいな。',
  '面白いな。',
  // な as part of vocabulary (中 = inside/naka)
  '中に入る。',
  '真ん中にいる。',
  // か in the middle of sentence (question particle, not sentence-final)
  'いつ行くか分からない。',
  // かしら (different particle - feminine "I wonder")
  'これでいいかしら。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
