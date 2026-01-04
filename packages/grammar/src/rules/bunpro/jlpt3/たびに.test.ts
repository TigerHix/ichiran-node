import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たびに.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences that should NOT match たびに
const negatives = [
  // ごとに - similar meaning but different grammar (each/every without "time/occasion")
  '一日ごとに目薬を差す。',
  '毎日２時間ごとに体を動かしている。',
  // おきに - "at intervals of" (different grammar)
  '３０分おきに休憩する。',
  '一時間おきに薬を飲む。',
  // とき - "when/at the time" (not "every time")
  '日本に来たとき、友達に会いました。',
  '子供のとき、よく遊びました。',
  // に as directional particle (not temporal)
  '病院に行く。',
  '学校に行きます。',
  // Simple noun + たび (without に)
  'たびを重ねる。',
  'このたびはありがとうございます。',
  // Noun + の + たび (without に) - "on this occasion"
  '今回のたびは大切にします。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
