import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がけに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative tests: similar but different usages that should NOT match
const negatives = [
  // 掛ける (transitive "to hang/suspend") - different meaning
  '彼は絵を壁に掛けた。',
  '眼鏡を掛けて本を読む。',
  // 掛かる (intransitive "to be hung/suspended")
  '絵が壁に掛かっている。',
  // 騎手 (kishu = jockey) - different word
  '彼は有名な騎手だ。',
  // 家計 (kakei = household budget) - different word
  '今月の家計を節約する。',
];

// Skip positives: GiNZA parsing limitations
// These are valid がけに usages that GiNZA parses in ways the rule cannot match.
const skipPositives: string[] = [
  // GiNZA tokenizes this sentence in an unexpected way
  '帰りがけに駅前のたこ焼き屋でたこ焼きを買った。'
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
