import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずに.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences that should NOT match
const negatives = [
  // ず (numeral counter) in different contexts
  '一ずつ', // One by one (counter, not auxiliary)
  '二ずつ', // Two by two (counter, not auxiliary)

  // Similar-looking patterns that aren't ずに
  '彼は知らないで言った。', // Regular negative ないで, not ずに
  '私は食べないで行った。', // Regular negative ないで, not ずに
  '勉強しないでテストを受けた。', // Regular negative ないで, not ずに

  // ず as part of a word (not auxiliary)
  'これは随分だ。', // ずいぶん (zuibun) - very, quite
  '図書館に行く。', // としょかん (toshokan) - library (ず sound)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
