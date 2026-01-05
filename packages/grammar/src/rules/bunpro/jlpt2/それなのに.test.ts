import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それなのに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the それなのに grammar rule
const negatives = [
  // Noun + な + のに (different grammar: "although [noun]")
  // Example: "Although he is a student..."
  '彼は学生なのに勉強しない。',
  '雨なのに外出します。',
  '子供なのに大人っぽい。',

  // それ alone (demonstrative pronoun, not the conjunction)
  'それは私の本です。',
  'それを見せてください。',
  'それを持っている。',

  // な + のに without それ (different grammar pattern)
  '静かなのにうるさいと言われた。',
  '便利なのに使われない。',

  // それ + other particles (different conjunctions)
  'それで終わりです。',
  'それにもかかわらず、',
  'それから行きます。',

  // それでも (similar but different conjunction: "even so")
  '雨が降っている。それでも行きます。',
  '疲れている。それでも働きます。',

  // しかし (formal "however")
  '勉強した。しかし、テストが難しかった。',

  // だけど (casual "but")
  '頑張った。だけど失敗した。',

  // くせに (critical "despite")
  '知っているくせに教えてくれない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
