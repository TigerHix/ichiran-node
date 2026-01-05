import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけあって.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the だけあって grammar rule
const negatives = [
  // だけ alone (without あって) - different grammar point
  'これだけあれば十分です。', // Only this is enough (just "dake")
  '時間だけが問題です。', // Only time is the problem (just "dake")

  // だけで (dake de) - "just by/only with" (JLPT4, different grammar)
  '見るだけで分かる。', // Can understand just by looking (dake de)
  'これだけでいいです。', // Just this is fine (dake de)
  '行くだけで参加できます。', // Can participate just by going (dake de)

  // だけに (dake ni) - similar meaning but different grammar (JLPT2)
  // Should be matched by separate だけに rule, not this one

  // Simple ある (aru) - "to exist/have" (not te-form)
  '彼はお金がある。', // He has money (simple aru)

  // あって (atte) in different contexts
  // e.g., 会ってあって (meet + te-form of au)
  // e.g., 合って (to match/fit + te-form)

  // だけあって with negative result (ungrammatical per grammar notes)
  // The grammar explicitly says not to use with negative reasons/results

  // だけあって at beginning of sentence (without preceding context)
  // This pattern requires the "because X" part to precede it

  // だけのことはあって (dake no koto wa aru) - sentence-final variant
  // Related but different grammar point (JLPT2)
  '優勝するなんて、さすが努力しただけのことはある。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
