import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あまりに.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negatives: sentences that should NOT match あまりに (exceedingly)
const negatives = [
  // あんまり + negative - "not too much" (different grammar: JLPT4 あまり-ない)
  'トマトはあんまり好きじゃない。',
  'この映画はあんまりおもしろくなかったね。',

  // あまり at end of sentence as noun (remainder/surplus)
  'まだ時間があまりある。',
  'お金があまり残っている。',

  // あまり + positive form without に (simple degree, not "exceedingly")
  'あまり暑い日だった。',

  // かなり / とても / なかなか - other degree adverbs (different grammar)
  'かなり忙しい。',
  'とてもおいしい。',
  'なかなか面白い映画だった。',

  // ずっと - comparison adverb (different grammar)
  'ずっと速い。',

  // すぎる without あまりに - standalone "too much"
  'このケーキは大きすぎて、食べられない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
