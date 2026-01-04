import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かけ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: similar but different usages that should NOT match
const negatives = [
  // 掛ける (transitive "to hang/suspend") - different meaning
  '彼は絵を壁に掛けた。',
  '眼鏡を掛けて本を読む。',
  // 騎手 (kishu = jockey) - different word
  '彼は有名な騎手だ。',
  // 家計 (kakei = household budget) - different word
  '今月の家計を節約する。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
