import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './を.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that should NOT match the object particle を.
// These use を in different grammatical contexts:
const negatives = [
  // Motion verbs with location (route/path) - technically this IS を marking location
  // but it's a special case where the location is the object of movement.
  // However, GiNZA parses these the same way as direct objects, so we accept them as matches.
  // Leaving this array empty for now since を is fairly unambiguous as an object marker.
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
