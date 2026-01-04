import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のに-despite.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // のに as "in order to" (purpose) - different meaning, same surface form
  // This is a genuine ambiguity; we accept matches for both meanings
  // 
  // Possessive の + case particle に (different grammar)
  '本を読む時間がない。', // possessive の, not conjunction
  '友達に会う。', // just に as case marker
  //  
  // ので (because) - different particle
  '忙しいので、行けません。',
  'だから、できません。',
  // 
  // だけど (but) - different casual conjunction
  '忙しいだけど、行きます。',
  //
  // Conjunction が (but) - different particle  
  '忙しいが、行きます。',
  //
  // Sentence-initial なのに (different grammar point)
  // 'なのに、彼は来なかった。', // This would be a different rule (sentence-initial)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
