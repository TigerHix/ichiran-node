import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './もう.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // まだ (mada) - antonym meaning "still" or "not yet"
  // We can't easily distinguish based on parse alone, but
  // sentences with まだ should not match the もう rule
  'まだ雨が降っている。',
  'まだ勉強しています。',
  'まだ昼ごはんを食べていない。',
  // もっと (motto) - different adverb meaning "more"
  // This has different lemma but similar sound
  'もっと勉強してください。',
  'もっと速く走りたい。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
