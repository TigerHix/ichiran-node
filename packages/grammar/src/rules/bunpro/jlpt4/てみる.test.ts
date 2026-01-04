import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てみる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match てみる
const negatives = [
  // みる as independent verb "to see/watch"
  'テレビを見る。',
  '映画を見ました。',
  // て-form without みる
  'ご飯を食べて、寝ました。',
  '買ってきました。',
  // てあげる (doing favor for someone)
  '本を貸してあげた。',
  '作ってあげましょう。',
  // てくれる (someone does favor for me)
  '彼が教えてくれた。',
  // てもらう (receive favor)
  '兄に手伝ってもらった。',
  // てしまう (completely/unintentionally)
  '食べてしまった。',
  '忘れてしまった。',
  // ておく (do in advance)
  '予約しておきます。',
  // ている (ongoing state)
  '本を読んでいる。',
  '雨が降っている。',
  // てある (resulting state)
  '黑板に書いてある。',
  // てくる (come and do / change over time)
  '買ってきました。',
  // ていく (go and do / continue into future)
  'これからも頑張っていく。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
