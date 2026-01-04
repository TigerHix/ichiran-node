import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './か-or.js';
import { BUNPRO_JLPT5 } from './index.js';

// Note: This rule matches か used as "or" (presenting alternatives) or as a question marker.
// It may also match か in other contexts like quotations or かどうか constructions.
// This is a known limitation due to GiNZA parsing patterns and the broad usage of か.

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get);
});
