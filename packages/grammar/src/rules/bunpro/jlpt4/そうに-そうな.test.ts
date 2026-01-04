import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そうに-そうな.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // そうです (hearsay, not seeming) - different grammar
  '彼は来るそうです。',
  '明日は雨だそうです。',
  // そう alone (without に/な particles)
  'そう思う。',
  'そうですね。',
  // そうだ as predicate (copula) not seeming
  'それはそうだ。',
  // Adjective stems ending in さ + そう (e.g., 大きそう) - different pattern
  // Note: These ARE valid for そう but require special handling
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
