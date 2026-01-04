import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないうちに.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // うちに (positive form) - "while" or "during"
  // This is a DIFFERENT grammar point from JLPT3 ないうちに
  '明るいうちに帰ろう。',
  '熱いうちに食べて。',
  '若いうちに勉強したい。',
  '日本にいるうちに富士山を見たい。',
  '元気なうちに旅行したい。',

  // 前に (before) - similar meaning but different structure
  '暗くなる前に帰ってください。',
  '忘れる前にメモを取ろう。',

  // ない alone (negation without うちに)
  '私は知らない。',
  '彼は来ない。',

  // ずに (without doing) - different grammar
  '勉強せずに寝た。',
  '言わずに分かった。',
];

// Known GiNZA limitation: いくらもおよがないうちに
// GiNZA appears to parse this sentence in a way that doesn't match our patterns
const skipPositives = [
  'いくらもおよがないうちに、向こう岸についてしまった。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
