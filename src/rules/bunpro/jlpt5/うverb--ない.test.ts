import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './うverb--ない.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases: similar patterns that should NOT match
const negatives = [
  // i-adjective negatives - ない is ADJ, not AUX
  '高くない',
  'おいしくない',
  '楽しくない',

  // ru-verb (ichidan) negatives - conjugationClass is 上一段 or 下一段, not 五段
  '食べない',
  '見ない',
  '寝ない',
  '起きない',  // okiru (to wake up) is ichidan, unlike okiru (to happen) which is godan

  // ある → ない - special case where ない is ADJ, not AUX
  'ケーキはない',
  'お金はない',
];

// Sentences that should be skipped because they don't actually contain
// the u-verb negative pattern (they have other issues)
const skipPositives = [
  // ある → ない is parsed as ADJ, not AUX (special case)
  'ケーキはない。 [[ケーキはないです。ケーキはありません。]]',
  'お金はない。 [[お金はないです。お金はありません。]]',

  // "木はあるかない" - the answer "あるかない" creates an invalid sentence
  // The correct example is in brackets: "歩かない"
  '木はあるかない。 [[木は歩かないです。木は歩きません。]]',

  // "ボールをうたない" - the answer "うたない" is hiragana reading, creates wrong verb
  // The correct example is in brackets: "打たない"
  'ボールをうたない。 [[ボールを打たないです。ボールを打ちません。]]',

  // "ロボットはやすまない" - hiragana reading, should be "休まない"
  'ロボットはやすまない。 [[ロボットは休まないです。ロボットは休みません。]]',

  // "それはしらない" - hiragana reading, should be "知らない"
  'それはしらない。 [[それは知らないです。それは知りません。]]',

  // "ペンギンはとばない" - hiragana reading, should be "飛ばない"
  'ペンギンはとばない。 [[ペンギンは飛ばないです。ペンギンは飛びません。]]',

  // "トミーははなさない" - hiragana reading, should be "話さない"
  'トミーははなさない。 [[トミーは話さないです。トミーは話しません。]]',

  // "今日はかえらない" - hiragana reading, should be "帰らない"
  '今日はかえらない。 [[今日は帰らないです。今日は帰りません。]]',

  // "今日はしなない" - hiragana reading, should be "死なない"
  '今日はしなない！ [[今日は死なないです！今日は死にません！]]',

  // "車はもちません" - uses polite form
  '[[ 車は持たない。 車は持たないです。 ]] 車はもちません。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
