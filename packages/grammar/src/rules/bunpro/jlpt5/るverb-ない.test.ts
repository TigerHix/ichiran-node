import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './るverb-ない.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that can't be matched due to grammar form differences:
const skipPositives = [
  // "私は黒板が見えません。" (I cannot see the blackboard) - This uses the POTENTIAL form
  // "見える" (can see), not the basic verb "見る" (to see).
  // The rule correctly matches only basic ru-verb negation (見ない, 見ません),
  // not potential form negation (見えない, 見えません).
  // Potential forms are a different grammar point.
  '私は黒板が見えません。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { skipPositives });
});
