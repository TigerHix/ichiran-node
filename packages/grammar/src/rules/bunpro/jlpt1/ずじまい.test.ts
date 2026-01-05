import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずじまい.js';
import { BUNPRO_JLPT1 } from './index.js';

// False positives: sentences that should NOT match
const negatives = [
  // ず (numeral counter) in different contexts
  '一ずつ', // One by one (counter, not auxiliary)
  '二ずつ', // Two by two (counter, not auxiliary)

  // Similar-looking patterns that aren't ずじまい
  '何も知らずにあんなこと言ってごめんなさい。', // ずに (without doing), not ずじまい
  '朝ごはんを食べずに仕事に行きました。', // ずに (without doing), not ずじまい
  '水を飲まずに運動をしていたから。', // ずに (without doing), not ずじまい

  // じまい in different contexts (if any exist)
  // Note: じまい is typically only used with ず in this pattern

  // ず as part of a word (not auxiliary)
  'これは随分だ。', // ずいぶん (zuibun) - very, quite
  '図書館に行く。', // としょかん (toshokan) - library (ず sound)
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
