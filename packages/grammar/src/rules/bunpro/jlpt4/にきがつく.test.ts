import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './にきがつく.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match にきがつく
const negatives = [
  // 気 as standalone noun (not part of 気がつく)
  // 気をつける (to be careful - different grammar)
  '気をつけてください。',
  '気をつけて帰ってきてね。',
  // 気がする (to have a feeling that - different grammar)
  '何か気がする。',
  '悪いことが起こる気がする。',
  // 気になる (to worry about/be on one's mind - different grammar)
  'そのことが気になっている。',
  '彼のことが気になっている。',
  // 気に入る (to like/please - different grammar)
  'その服はとても気に入った。',
  // Similar but different patterns
  // につく (to arrive at - different verb)
  '東京駅についた。',
  '家についたとき、雨が降っていた。',
  // 気がする vs 気がつく - different verb
  '彼が来た気がする。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
