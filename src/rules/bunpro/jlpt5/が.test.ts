import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './が.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences where が means "but" (conjunction), not subject marker.
// These have ONLY one が particle, so any match is a false positive.
const negatives = [
  // Conjunction が - connects two clauses with "but"
  '毎日走るが、運動はきらいです。',
  'お金は大切だが、時間も大切だ。',
  'このカレーは辛いが、美味しい。',
  '大変です。ですが、面白いです。',
  '車は便利だが、危ない。',
  '難しいですが、頑張ります。',
  'まあ、綺麗ですが。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
