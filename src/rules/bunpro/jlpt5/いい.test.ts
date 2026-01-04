import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いい.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Other adjectives that are not "good"
  'これは高い本です。',
  'この部屋は広い。',
  '今日は寒い。',
  'この料理はおいしい。',
  // 形容詞-一般 that's not the "good" adjective
  'いろいろな本がある。',
  // Na-adjective ending in い (きれい)
  '水はきれいです。',
  // Other uses of いい (if any appear in context)
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
