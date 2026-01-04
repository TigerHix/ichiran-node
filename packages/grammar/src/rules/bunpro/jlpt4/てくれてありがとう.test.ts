import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てくれてありがとう.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: sentences that should NOT match てくれてありがとう
const negatives = [
  // てform verb alone without くれてありがとう
  '本を読んでいる。',
  'ご飯を作って食べた。',

  // てform + ありがとう but no くれて (different grammar: thanking for the action itself)
  // Note: This might still match due to the pattern, but linguistically different
  '来てありがとう。',

  // て + もらって + ありがとう (receiving focus, not doing-for-me focus)
  '本を買ってもらってありがとう。',

  // て + あげて + ありがとう (doing-for-someone-else, not for-me)
  '手伝ってあげてありがとう。',

  // くれる without て-form
  '彼がくれる。',

  // Simple ありがとう without grammar pattern
  'ありがとう。',
  'ありがとうございます。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
