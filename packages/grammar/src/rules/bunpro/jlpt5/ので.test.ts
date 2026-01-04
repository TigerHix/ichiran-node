import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ので.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // から-because (reason) - different particle
  '雨だから行かない。',
  'おいしいから食べた。',
  '寒いからコートを着る。',
  '彼女は中国人だから、中国語が話せる。',
  '便利だから使っています。',
  '行ったから疲れた。',
  '高いから買わない。',
  '学生だから勉強する。',
  // ですから (polite because)
  '明日は休みですから、今日は早く帰ります。',
  // から (from/source) - uses dep=case, not dep=mark
  '10時から始めます。',
  '東京から行きます。',
  '友達から本を借りました。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
