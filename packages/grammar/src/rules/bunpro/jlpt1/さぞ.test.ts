import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さぞ.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Other adverbs with similar meaning but different surface form
  'きっと大丈夫だろう。',
  'とても嬉しいことでしょう。',
  'いかに難しい問題だろう。',

  // さっぱり - different meaning (completely/not at all)
  'さっぱりしない。',
  'さっぱり分からない。',

  // さっと - different meaning (quickly/suddenly)
  'さっと行った。',
  '彼はさっと部屋を出た。',

  // さぞ followed by quote marker (と) + verb - not conjecture
  // This would be: "He said さぞ..." (not the grammar pattern)
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
