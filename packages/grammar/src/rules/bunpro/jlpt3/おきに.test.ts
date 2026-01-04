import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おきに.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences that should NOT match おきに
const negatives = [
  // ごとに - similar meaning but different grammar (every/each without interval emphasis)
  '一日ごとに目薬を差す。',
  '毎日２時間ごとに体を動かしている。',
  // たびに - "whenever/each time" (different grammar)
  '会うたびに彼は冗談を言う。',
  '行くたびに雨が降る。',
  // に as directional particle (not interval suffix)
  '病院に行く。',
  '学校に行きます。',
  // Simple に particle without おき
  'この薬は飲んでください。',
  '温度を測ってください。',
  // あたり - "per X" (different suffix)
  '一人あたり一千円。',
  // ずつ - "each X at a time" (different suffix)
  '二つずつ食べてください。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
