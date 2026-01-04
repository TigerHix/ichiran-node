import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あるいは.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: sentences that should NOT match the conjunction pattern
const negatives = [
  // Verb ある + particle は as separate tokens (different grammar)
  'お金がある。',
  '本がある。本は新しい。',
  '問題がある。はい、わかりました。',
  '彼はお金がある。はい、それはいいことです。',
  // ある as existence verb with case markers
  '私があることを知っている。',
  '時間があるから行きます。',
  '友達があるから大丈夫です。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
