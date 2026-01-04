import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おおよそ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // だいたい (daitai) - more casual synonym (different grammar point)
  // Can be distinguished by lemma: だいたい vs おおよそ/およそ
  'だいたい百人来た。',
  'だいたいでいいから、教えてください。',

  // 大体（だいたい） - written in kanji, more casual
  '大体の意味はわかりました。',
  '大体の仕事は終わりました。',
  'その辺は大体の人が知っている。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
