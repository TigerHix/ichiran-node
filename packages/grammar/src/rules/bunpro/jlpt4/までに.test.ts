import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './までに.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases: sentences that should NOT match this rule
const negatives = [
  // まで without に (means "until" - duration, not deadline)
  '来月までレポートを書く。',
  '８時までテレビを見る。',
  '１０時まで待ってください。',
  '昨日は朝から晩まで雨が降っていました。',
  // Spatial まで (as far as/to, not temporal "by")
  '駅まで歩いて行きます。',
  'ここまで来てください。',
  '東京まで新幹線で行きます。',
  // に without まで (just "at" or "to", not "by")
  '１０時に帰ります。',
  '明日に行きます。',
  '学校に来てください。',
  // Similar but different grammar patterns
  'までは',  // wa topic marker, not ni
  'までも',  // mo "even", not ni
  'までが',  // ga subject marker, not ni
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
