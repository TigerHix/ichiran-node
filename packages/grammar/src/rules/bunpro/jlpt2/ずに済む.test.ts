import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずに済む.js';
import { BUNPRO_JLPT2 } from './index.js';

// False positives: sentences that should NOT match ずに済む
const negatives = [
  // ず (numeral counter) in different contexts
  '一ずつ行く。', // One by one (counter, not auxiliary)
  '二ずつ食べる。', // Two by two (counter, not auxiliary)

  // Simple ずに (without doing) NOT followed by 済む
  '何も知らずにあんなこと言ってごめんなさい。', // Sorry for saying that without knowing (no 済む)
  '朝ごはんを食べずに仕事に行きました。', // Went to work without eating breakfast (no 済む)
  '水を飲まずに運動をしていたから。', // Was exercising without drinking water (no 済む)
  '値段を見ずに買ったら大変なことになった。', // Bought without checking price (no 済む)

  // Regular negative forms with ないで/なくて + 済む (not ずに)
  // These are alternative forms but should be matched by different rules
  '初期費用を支払わないで済む。', // Get by without paying (using ないで, not ずに)
  '怪我をしなくて済んだ。', // Got by without getting injured (using なくて, not ずに)
  'テストを受けないで済んだ。', // Got by without taking test (using ないで, not ずに)

  // ず as part of a word (not auxiliary)
  'これは随分だ。', // ずいぶん (zuibun) - very, quite
  '図書館に行く。', // としょかん (toshokan) - library (ず sound)

  // 済む used in different contexts (not with ずに)
  '金で済む問題だ。', // Problem that can be solved with money (different usage)
  'これで済みますか。', // Will this be enough/is this finished? (different usage)
  '事は済んだ。', // The matter is finished (different usage)

  // Similar-looking patterns that aren't ずに済む
  'せずにはいられない。', // Can't help but do (different grammar: ずにはいられない)
  '忘れずに届けてくれて。', // Deliver without forgetting (no 済む)
  '休まず、一日中ゲームをやり続けた。', // Played games all day without resting (no 済む)
];

// Sentences that should match but are skipped due to known limitations
const skipPositives = [
  // Alternative forms using ないで/なくて/なしで instead of ずに
  // These are valid variants of the ずに済む grammar point but use different negative forms
  // They could be handled by separate rules if needed
  '今月契約すると初期費用を支払わないですむので、今月中に契約することをお勧めします。', // Uses ないで instead of ずに
  '今年の冬は去年より暖かかったので、ヒーターなしですみました。', // Uses なしで instead of ずに (noun + suffix)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
