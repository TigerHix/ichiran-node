import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './い-adjective-predicate.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match
const negatives = [
  // い-adjective modifying a noun (not predicate)
  'かわいい猫',
  '新しい家',
  '暑い日',
  // な-adjective with だ (different grammar point)
  '静かだ',
  'きれいだ',
  // な-adjective with です (different grammar point)
  '静かです',
  'きれいです',
  // 嫌い (actually na-adjective, despite ending in い)
  '私は嫌い。',
  '私は嫌いです。',
  // Various い-adjectives modifying nouns
  '高い山',
  'おいしい料理',
  '面白い本',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
