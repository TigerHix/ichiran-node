import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かたがた.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Similar patterns that should NOT match
  // がてら (casual version, different grammar)
  '散歩がてら、本を買ってきた。',
  '買い物がてら、友達に会った。',

  // を兼ねて (similar but less formal)
  'ダイエットも兼ねて毎朝散歩する。',
  '教室を兼ねた倉庫として使っている。',

  // ついでに (casual "while" expression)
  '買い物のついでに郵便局に寄る。',
  '東京に行ったついでに友達を訪ねた。',

  // Regular compound nouns with かた (different meaning)
  '彼はいいかたの人です。',
  '向こうの方から来た。',

  // Independent use of nouns without かたがた
  'お礼を言いました。',
  '報告をしました。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
