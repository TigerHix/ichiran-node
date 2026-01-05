import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずに済む.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ずに済む grammar rule
const negatives = [
  // Positive forms (with て instead of ずに) - opposite meaning
  'これをして済むならいいんだけど。', // して済む (manage by doing, not without)
  '謝って済む問題じゃない。', // て済む (not ずに済む)
  'お金を払って済むことだ。', // て済む (positive form)
  '謝って済むと思っているのか。', // て済む (positive form)

  // Just ずに without 済む - different grammar (JLPT3 ずに)
  '朝ごはんを食べずに学校に行った。', // Just "without eating"
  '何も知らずに言ってごめん。', // Just "without knowing"
  '傘を持たずに出かけた。', // Just "without taking umbrella"
  '彼は寝ずに仕事を続けた。', // Just "without sleeping"

  // ないで済む variants (modern form, not classical ずに)
  // These are alternate forms but we're testing only the classical ずに form
  '初期費用を支払わないで済むので、', // ないで (modern form)
  '今年の冬は去年より暖かかったので、ヒーターなしで済みました。', // なしで (noun + variant)
  '彼は警察から、捕まらないで済んだ。', // ないで済んだ (modern)

  // なくて済む variants (te-form of negative)
  '支払わなくて済む。', // なくて (te-form)
  '使わなくて済んだ。', // なくて済んだ (modern)

  // Regular 済む without ずに (different meanings)
  'この問題は済んだ。', // Just 済んだ (is finished/resolved)
  'それで済むことじゃない。', // 済む (sufficient/okay)
  '事は済んだ。', // 済んだ (completed)

  // ず as counter suffix (numeral)
  '一ずつ話してください。', // ずつ (counter, not auxiliary)
  '二ずつ分けた。', // ずつ (counter)

  // Similar-looking but unrelated
  'これは図書館です。', // としょかん (toshokan - library, has ず sound)
  '随分いいですね。', // ずいぶん (zuibun - very)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
