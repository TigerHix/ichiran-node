import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずつ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences that should NOT match ずつ
const negatives = [
  // おきに - "at intervals of" (time/distance intervals)
  '一日おきに目薬を差す。',
  '３０分おきに体を動かしている。',
  // ごとに - "each/every" (without interval emphasis)
  '一日ごとに温度を測る。',
  '３０分ごとに体を動かす。',
  // たびに - "whenever/each time"
  '会うたびに彼は冗談を言う。',
  // あたり - "per X" (apiece, price-based)
  '一人あたり一千円。',
  // ずっと - "continuously" (different word)
  'ずっと待っています。',
  // に as simple particle
  '学校に行きます。',
  '病院に行く。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
