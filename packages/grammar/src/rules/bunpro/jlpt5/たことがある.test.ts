import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たことがある.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Dictionary form + ことがある (occasional occurrence, different grammar)
  '雨が降ることがある。',
  '遅刻することがある。',
  '彼に会うことがある。',
  // Verb in te-form + いる + ことがある (continuous state + experience, different structure)
  '彼を知っていることがある。',
  // Noun + がある (simple existence, not experience)
  '本がある。',
  'お金がある。',
  // Verb + た + noun (past tense modifying noun, not koto-ga-aru pattern)
  '食べたリンゴ。',
  '買った本。',
  // Verb + て + ある (resultative state, different grammar)
  '書いてある。',
  '置いてある。',
  // Verb + た + ことが + ある but with wrong particle (は instead of が)
  '行ったことはある。',
  // Different nominalizer (の instead of こと)
  '行ったのがある。',
  // Verb + た + 後 + が + ある (different structure)
  '食べた後がある。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
