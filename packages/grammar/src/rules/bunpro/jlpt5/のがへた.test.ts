import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のがへた.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // のが上手 (good at - antonym, should not match へた)
  '彼は歌うのが上手です。',
  '漢字を書くのが上手だ。',
  // 下手 without verb+のが structure
  '下手だ。',
  '下手ですね。',
  // Different nominalization: のが好き (to like doing)
  '私はサッカーをするのが好きです。',
  '歌うのが好きだ。',
  // Different nominalization: のが嫌い (to hate doing)
  '私は掃除をするのが嫌いです。',
  '勉強するのがきらいだ。',
  // が as subject marker without の (not nominalization)
  '私が下手です。',
  '彼が下手だ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
