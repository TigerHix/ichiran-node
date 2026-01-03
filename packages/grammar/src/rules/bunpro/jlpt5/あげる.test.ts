import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あげる.js';
import { BUNPRO_JLPT5 } from './index.js';

// Note: This rule matches both main verb あげる (to give) and the auxiliary てあげる
// (do for someone) construction. The test data for this grammar point only contains
// main verb examples, so this is not an issue. Distinguishing between main verb
// and auxiliary uses would require negating dependency edges, which the current DSL
// does not support well (negations are applied per-clause, not as groups).

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get);
});
