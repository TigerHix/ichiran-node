import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './な-adjectives.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // い-adjectives (true i-adjectives ending in い)
  '寒い。',
  'たのしい。',
  'おいしいです。',
  '高いです。',

  // Nouns (not na-adjectives)
  '学生です。',
  '先生だ。',
  '本です。',

  // Verb forms (should not match)
  '行かない。',
  '食べない。',

  // Noun + の + noun (nominalizer, not na-adj)
  '私の本です。',
  '先生の車。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
