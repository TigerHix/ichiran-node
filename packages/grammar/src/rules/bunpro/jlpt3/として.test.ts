import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './として.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // にとって (ni totte) - "for" (different grammar point)
  // This takes the perspective OF the noun, not AS the noun
  '私にとって、これは大切です。',

  // にしては (ni shite wa) - "considering that..." (different grammar)
  // Used for unexpected qualities
  '彼は新人にしては、よくやる。',

  // と alone (quotational particle without して)
  '彼は「行く」と言った。',

  // して as te-form conjunction (without と)
  '宿題をして、遊びに行った。',

  // Locative として (at a time/place - rare but exists)
  // Note: These would need different structural analysis
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
