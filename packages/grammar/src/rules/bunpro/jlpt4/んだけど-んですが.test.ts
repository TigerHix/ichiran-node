import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './んだけど-んですが.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Subject particle が (not conjunction)
  '私が学生です。',
  '彼が来ました。',
  // だけど/ですが WITHOUT explanatory ん/の (different grammar - だが・ですが)
  'それは簡単だけど、時間がかかる。',
  'それは簡単ですが、時間がかかります。',
  // Simple けど (casual conjunction, no ん/の)
  '行きたいけど、時間がない。',
  // Simple が (formal conjunction, no ん/の)
  '行きたいが、時間がない。',
  // んだけ/んです as explanatory sentence end (no conjunction particle)
  'それは高いんだ。',
  'それは高いんです。',
  // のだ as explanatory sentence end (no conjunction)
  '私は行くんだ。',
  // Conjunctions without explanatory ん/の
  'しかし、時間がない。',
  'だから、行けない。',
  'それでも、行きたい。',
  // んけど (casual, but missing だ - different grammar)
  '行きたいんけど、時間がない。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
