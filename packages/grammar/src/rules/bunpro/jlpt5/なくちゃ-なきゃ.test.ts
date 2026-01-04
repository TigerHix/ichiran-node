import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なくちゃ-なきゃ.js';
import { BUNPRO_JLPT5 } from './index.js';

// False positives: sentences with similar patterns that should NOT match
const negatives = [
  // Negative verb without obligation marker
  '食べない。',
  '行かない。',

  // Negative verb + くて (te-form of negation, conjunction)
  '食べなくて、行った。',

  // Conditional form (-kara/-ba)
  '食べなければ行く。',

  // Negative verb + other auxiliaries
  '食べなかった。',
  '食べなくなる。',
  '食べさせない。',

  // Similar but different grammar
  '食べないで。', // Negative te-form request
  '食べなければなりません。', // Full obligation form (not contracted)

  // なくて as adverbial form (not obligation)
  '彼は来なくて、困った。',

  // Separate clauses (verb+nai+kya but separate grammatical structures)
  '何もしない。きゃあ、いいや。',

  // Other casual contractions (different grammar)
  'しちゃった。', // Completed action
  'じゃない。', // Negation
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
