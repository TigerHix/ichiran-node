import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そうだ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: patterns that should NOT match the hearsay そうだ
const negatives = [
  // Appearance/Conjecture form (样態) - uses STEM form, not dictionary form
  // This is the JLPT4 そうだ rule, different from hearsay
  '雨が降りそうだ。', // looks like it will rain (stem form)
  'このケーキは美味しそうだ。', // looks delicious (dropped い)
  '彼はお金持ちそうだ。', // looks rich (noun + direct そうだ)
  '今日は寒そうだ。', // looks cold (dropped い)

  // Verb stem + そうだ (appearance/conjecture)
  '彼は来そうだ。', // looks like he will come
  '試合に勝ちそうだ。', // looks like we will win

  // い-adjective stem (dropped い) + そうだ (appearance/conjecture)
  'この料理は熱そうだ。', // looks hot
  '彼は悲しそうだ。', // looks sad

  // Noun/な-adj direct + そうだ (appearance/conjecture - no だ before そう)
  '彼は元気そうだ。', // looks healthy
  'この店は人気そうだ。', // looks popular

  // Different grammar patterns that might look similar
  // と言うそう (quotation + different structure)
  // Note: these may not exist in natural Japanese, but listing for clarity

  // Simple そう (so/very - adverb)
  '彼はそう思っている。', // he thinks so
  'そうですね。', // that's right

  // そうして (and then)
  'そうして、彼は去った。', // and then he left

  // そうした (did so)
  '彼はそうした。', // he did so
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
