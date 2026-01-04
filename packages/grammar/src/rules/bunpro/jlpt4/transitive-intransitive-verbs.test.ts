import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './transitive-intransitive-verbs.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match transitive-intransitive-verbs
const negatives = [
  // Same verbs but different particle (should not match if particle is wrong)
  // These use the wrong particle for the verb type
  'バナナが落とす',     // Uses が with transitive verb (unusual)
  'バナナを落ちる',     // Uses を with intransitive verb (unusual)

  // Similar verbs that are not in the pair list
  '本を読む',          // yomu is transitive but not in our list
  '本が読まれる',      // passive form, different grammar

  // する/なる with different particles (context-dependent)
  '勉強する',          // する without direct object
  '大人になる',        // なる without が
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
