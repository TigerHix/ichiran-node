import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './じゃないか.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that should NOT match (similar patterns but different grammar)
const negatives = [
  // Simple negation without question particle (not じゃないか)
  'これは高くない。',
  '彼は学生ではない。',
  // Plain question with か (not じゃないか)
  'これは高いですか。',
  'あなたは学生ですか。',
  // ではない (negation) without か
  'これは本ではない。',
  '彼は日本人ではない。',
  // じゃない (negation) without か
  'それはペンじゃない。',
  '今日は休みじゃない。',
  // Different sentence endings (not か)
  '高いんじゃない。',
  '綺麗じゃない。',
  // じゃないて (different pattern)
  'やるじゃないて。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
