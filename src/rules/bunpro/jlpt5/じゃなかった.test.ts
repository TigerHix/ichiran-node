import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './じゃなかった.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Negative non-past (じゃない / ではない)
  'それは猫じゃない。',
  'これは本ではない。',
  // Positive past (だった / でした)
  'それは猫だった。',
  'これは本でした。',
  // I-adjective negative past (くなかった) - different grammar
  'この料理は美味しくなかった。',
  '昨日は寒くなかった。',
  // I-adjective (not na-adj)
  'この部屋は狭い。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
