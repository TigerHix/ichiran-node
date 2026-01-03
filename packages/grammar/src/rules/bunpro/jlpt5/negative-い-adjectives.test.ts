import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './negative-い-adjectives.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Verb negation (verb + ない) - should NOT match
  '行かない。',
  '食べない。',

  // Positive i-adjectives (ending in い) - should NOT match
  '寒い。',
  'たのしい。',

  // na-adjective negation with じゃない - should NOT match
  '静かじゃない。',
  '学生じゃない。',

  // きれい (fake i-adj, actually na-adj) - should NOT match
  'きれいじゃない。',
  'きれくない', // not a real word
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
