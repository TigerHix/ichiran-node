import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './くれる.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match くれる
const negatives = [
  // あげる (ageru) - opposite direction (giving away from speaker)
  '私は彼に本をあげる。',
  '彼は弟にお菓子をあげた。',
  '友達にプレゼントをあげます。',

  // もらう (morau) - receiving (different verb)
  '私は彼から本をもらう。',
  '彼にプレゼントをもらった。',

  // Other verbs that shouldn't match
  '彼は毎来这里る。', // くる (kuru) - to come
  '雨が降る。', // ふる (furu) - to rain
  '彼が行く。', // いく (iku) - to go
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
