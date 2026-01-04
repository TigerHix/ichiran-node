import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ついでに.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // うちに - temporal "while/during", different meaning
  '寝ているうちに電話がかかってきた。',
  '若いうちにたくさん勉強しておいてください。',
  '日本にいるうちに京都を訪ねたい。',
  // 間に - temporal "during/between", different meaning
  '昼休みの間に昼寝をした。',
  '授業の間に眠ってしまった。',
  '両親がいない間にパーティーをした。',
  // ときに - "at the time of", different meaning
  '東京に来たときに友達に会った。',
  '帰るときにメールをください。',
  // Similar "〜てに" patterns that aren't ついでに
  '重いので持ち上げられない。', // で (reason)
  '電車で行きます。', // で (means)
  '公園で遊びます。', // で (location)
  // ついで without に (noun form, not grammar pattern)
  'ついでがあったので頼みました。',
  'ついでを利用する。',
  // ついに - "finally" (different word)
  'ついに完成しました。',
  'ついに雨が降ってきた。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
