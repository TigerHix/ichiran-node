import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だ.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: だ should NOT match in these cases
const negatives = [
  // I-adjectives + だ is ungrammatical (per caution in Bunpro data)
  '大きいだ。',
  '高いだ。',
  '新しいだ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
