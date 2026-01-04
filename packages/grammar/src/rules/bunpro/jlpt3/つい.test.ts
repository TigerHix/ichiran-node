import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './つい.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ついに (finally) - different word
  'ついに会えた。',
  'ついに終わった。',
  'ついに成功した。',

  // Note: つい as "directly/soon" (spatial/temporal proximity) like:
  // - "ついそこにあった" (it was right there)
  // - "つい先まで来た" (came just before)
  // These are structurally identical to the target pattern and cannot
  // be distinguished syntactically. The difference is semantic/contextual.
  // In practice, these uses are less common in the JLPT3 context.
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
