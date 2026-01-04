import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './というのは.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // という (JLPT3) - "called X" pattern
  // Noun + という + Noun (different structure, no のは)
  'ポケモンというゲームがある。',
  '佐藤浩一という人を知っていますか。',

  // かというと (JLPT3) - "if we ask" pattern
  // Requires question particle か before と
  'なぜ行かなかったかというと、忙しかったからです。',

  // ということ (JLPT4) - "the fact that" pattern
  // Ends with こと (noun) not のは
  '彼が来るということだ。',
  '結婚したということです。',

  // Plain は topic marker (not というのは)
  '私は学生です。',
  '今日は良い天気です。',

  // Simple quotational と (not いう follows)
  '「こんにちは」と言いました。',
  '彼が行くと思います。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
