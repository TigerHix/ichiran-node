import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './つもりだ.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Noun + のつもり (pretending/acting as) - no verb clause
  '彼は先生のつもりだ。',
  '子供のつもりで遊んだ。',
  // つもり as standalone noun (not in grammar pattern)
  'つもりがない。',
  // Different つもり: "積もり" (accumulation)
  '雪が積もりそうだ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
