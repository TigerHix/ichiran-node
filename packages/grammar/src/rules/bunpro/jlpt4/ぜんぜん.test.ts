import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ぜんぜん.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 全然 as noun modifying another expression (not this grammar point)
  // This is rare but can occur in specific contexts

  // 全然 with positive emphasis in formal/written contexts
  // (should use different expressions like まったく or 完全に)
  // Note: Modern casual speech accepts positive 全然, but formal writing
  // generally does not. Our rule correctly matches the positive pattern
  // for casual usage.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
