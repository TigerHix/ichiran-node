import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かと思ったら-かと思うと.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // 思う (simple thought) - different meaning
  '彼が来ると思う。',
  'そう思う。',
  // 思った (simple past thought) - different meaning
  '彼女はそう思った。',
  // と思う (quoting) - different grammar
  '彼は「行く」と思った。',
  '「明日は雨だ」と思う。',
  // としたら (conditional) - different grammar point
  'もし雨が降ったら、行きません。',
  'そんなに高いとしたら、買わない。',
  // かと (quoting with uncertainty) - incomplete
  '何か言うかと。',
  // 思いきや (contrary to expectation) - different grammar
  '助かると思いきや、状況は悪化した。',
  // なり (as soon as) - different grammar
  'ドアを開けるなり走り出した。',
  // たとたんに (the moment) - different grammar
  'ドアを開けたとたんに猫が飛び出した。',
  // Simple past tense without grammar pattern
  '家に着いた。',
  '雨が止んだ。',
  // か〜ないかのうちに (similar but different)
  'ベルが鳴るか鳴らないかのうちに学生が飛び出した。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
