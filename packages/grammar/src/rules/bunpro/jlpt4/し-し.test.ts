import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './し-し.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // し as a noun (death, four) - not the particle
  // These are contextually rare but possible

  // Simple noun without だ (not listing reasons)
  // はし (chopsticks/bridge) - completely different word
  'はしでご飯を食べる。',

  // Noun + し without だ (not a conj particle pattern)
  // Most nouns need だ + し for this grammar point
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
