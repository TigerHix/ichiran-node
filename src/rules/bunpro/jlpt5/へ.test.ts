import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './へ.js';
import { BUNPRO_JLPT5 } from './index.js';

// No negative tests needed for this simple particle rule.
// The particle へ (pronounced 'e') is consistently used as a directional case marker.
// Unlike が or は, it doesn't have other grammatical functions that would cause ambiguity.

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get);
});
