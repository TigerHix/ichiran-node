import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てから.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // たから (reason: because/since) - past tense + reason marker
  '牛丼の大盛りを食べたから、お腹いっぱい。',
  // 辞書形から (reason: because/since) - dictionary form + reason marker
  '今夜は勉強をするから、遊ばない。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
