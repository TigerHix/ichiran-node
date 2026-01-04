import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ようにする.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match ようにする
const negatives = [
  // Plain する without ように (just "to do")
  '勉強する。',
  '宿題をする。',
  '毎日運動する。',
  // ようになる (change of state: "came to be that..." or "became able to...")
  '日本語を話せるようになった。',
  '泳げるようになりました。',
  '朝早く起きるようになった。',
  // ようだ (seems like / looks like)
  '雨が降るようだ。',
  '彼は疲れているようです。',
  // ように without する (purpose: "so that" without volition)
  // This is handled by a different rule (単に ように)
  '風が入るように窓を開ける。',
  '遅れないように早く起きる。',
  // にする (to decide on / to choose)
  'これにする。',
  'ハンバーガーにします。',
  '赤にしました。',
  // ことにする (to decide to)
  '卒業することにした。',
  '日本に行くことにします。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
