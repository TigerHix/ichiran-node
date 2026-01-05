import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しかも.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the しかも grammar rule
const negatives = [
  // Similar but different conjunctions
  'それに、彼も来る。',
  'その上、雨も降ってきた。',
  'また、明日も雨だ。',
  'さらに、詳細を説明します。',
  'なお、調査を続けます。',

  // Particles and similar-looking fragments
  'しかしあの人は来なかった。',
  'その人はしかしうるさい。',
  'しかしながら、頑張ります。',

  // して (shite) - "doing" (te-form of する)
  '彼は勉強している。',

  // かも (kamo) - "might be"
  '明日は雨かもしれない。',
  '彼は忙しいかもしれない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
