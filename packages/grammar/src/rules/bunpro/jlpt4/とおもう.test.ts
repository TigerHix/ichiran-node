import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とおもう.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: similar-looking patterns that should NOT match
const negatives = [
  // Conditional と (when/if)
  '春になると花が咲く。',
  '右に行くと駅があります。',
  // Accompaniment と (with)
  '友達と行く。',
  '彼と話す。',
  // Comparison と
  'これはそれと同じだ。',
  // Quotation with other verbs (say, tell, etc.)
  '彼は行くと言った。',
  '彼女は来ると言いました。',
  // と as 'and'
  'りんごとバナナを買う。',
  // Simple particle と in other contexts
  '東京に行きます。',
  // Note: Direct quotation 「...」と思う is also valid and matches this rule.
  // The distinction between direct and indirect quotation is contextual.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
