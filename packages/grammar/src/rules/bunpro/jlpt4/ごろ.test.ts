import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ごろ.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases: sentences that should NOT match this rule
const negatives = [
  // くらい/ぐらい (degree/extent, not time)
  '１２時くらいに帰る。',
  '１０分くらい待ってください。',
  '子供のくらいはよく親と動物園へ行きました。',
  '二メートルくらいの所に印をつけてください。',
  // ごと/ごとに (each/every, not "around")
  '３時間ごとに薬を飲む。',
  '日曜日ごとの集まりがあります。',
  // とき (when/at the time, not "around")
  '子供の時によく遊びました。',
  '日本に来た時、雨が降っていました。',
  // ところ (at the point/time, not "around")
  '今のところ、問題はない。',
  '良いところを見つけた。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
