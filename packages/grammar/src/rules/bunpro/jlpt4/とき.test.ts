import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とき.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases: sentences that should NOT match this rule
const negatives = [
  // ごろ (around/approximate, not "when")
  '１２時ごろに帰る。',
  '１０分ごろ待ってください。',
  '子供のごろはよく親と動物園へ行きました。',
  // あいだ (while/during, not "when")
  '日本にいるあいだに、京都に行きたい。',
  '寝ているあいだに電話が鳴った。',
  '子供のあいだはよく親と動物園へ行きました。',
  // あいだに (while/during, not "when")
  '日本にいるあいだに、京都に行きたい。',
  '寝ているあいだに電話が鳴った。',
  // さい (occasion/case, different nuance)
  'このさいはquietにしてください。',
  '旅行のさい、カメラを買いました。',
  // さいに (on the occasion of, different nuance)
  'このさいにquietにしてください。',
  '旅行のさいに、カメラを買いました。',
  // ところ (at the point/time, different nuance)
  '今のところ、問題はない。',
  '良いところを見つけた。',
  // 今 (time word not followed by とき)
  '今行きます。',
  // 昨日 (time word not followed by とき)
  '昨日行きました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
