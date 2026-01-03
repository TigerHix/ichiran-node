import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そこ.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: そこ should NOT match these
const negatives = [
  // Other demonstrative pronouns for locations
  'ここです。',
  'ここがいい。',
  'あそこです。',
  'あそこが遠い。',

  // Demonstrative pronouns for things (not places)
  'これです。',
  'それです。',
  'あれです。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
