import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './は.js';
import { BUNPRO_JLPT5 } from './index.js';

// Note: The は particle is a topic marker. In Japanese, は appears in many
// compound particles (には, では, とは, からは, よりは, etc.) where it is
// still functioning as a topic marker linguistically.
//
// This rule matches ALL topic-marker は usage, including in compounds.
// More specific grammar rules (like JLPT3 では-それでは-じゃあ) handle
// compound particles as distinct patterns with different semantics.
//
// Therefore, we don't have negatives for compound particles - they're
// valid matches of the は topic marker. What we DO want to exclude are
// uses of は that are NOT topic markers at all.

const negatives = [
  // No good negative examples exist - は is almost exclusively a topic marker
  // Unlike が (which can mean "but"), は doesn't have alternative grammatical
  // functions that would create false positives in this context.
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
