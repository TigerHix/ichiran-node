import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './その結果.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences that should NOT match
const negatives = [
  // Simple noun phrase without connective meaning
  'その結果を報告します。',
  'その結果を見てみましょう。',
  // その followed by different word
  'その原因は不明です。',
  'その方法で試してみます。',
  // 結果 without その
  '結果として成功しました。',
  '良い結果が得られました。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
