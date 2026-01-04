import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './から.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // から-because (reason) - uses dep=mark, not dep=case
  '雨だから行かない。',
  'おいしいから食べた。',
  '寒いからコートを着る。',
  '彼女は中国人だから、中国語が話せる。',
  '便利だから使っています。',
  // ですから (polite because)
  '明日は休みですから、今日は早く帰ります。',
  // Verb/Adj + から (reason pattern)
  '行ったから疲れた。',
  '高いから買わない。',
  // Noun + だ + から (because pattern, not from)
  '学生だから勉強する。',
  '会社だから忙しい。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
