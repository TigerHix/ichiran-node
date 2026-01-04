import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './こそ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences that should NOT match こそ
const negatives = [
  // Regular こ as part of words (not こそ particle)
  'ここに来てください。',
  'こっちに来て。',
  'これを見て。',
  // Different particles (が, は, を)
  '私が学生です。',
  '私は学生です。',
  '私を見て。',
  // 子 as 'child' (not こそ)
  '子が遊んでいる。',
  // こ as sentence-initial element (not emphatic)
  '今日は良い天気です。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
