import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しかも.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // しかし (however) - different conjunction
  'しかし、彼は来なかった。',
  '頑張った。しかし、ダメだった。',

  // しか + particle combinations (not しかも)
  'お金しかない。',
  '今日しかできない。',
  '彼にしか話せない。',

  // しが (also possible) - different word
  'それはしがないことだ。',

  // しま (verb stem) - different word
  '本を読みしまった。',

  // その他 (sonohoka) - different word
  'その他には何もない。',

  // Similar conjunctions that should NOT match しかも
  'そして、彼は来た。',           // soshite (and then)
  'その上、彼は金持ちだ。',        // sono ue (moreover)
  'さらに、驚いたことがあった。',  // sara ni (furthermore)
  'それに、とても安い。',          // soreni (besides)
  'また、明日も雨です。',          // mata (also/again)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
