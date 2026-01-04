import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ながら.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // ながらも (different grammar point - "even though")
  '古いながらも中は綺麗だ。',
  '狭いながらも楽しい家だ。',
  // ながら with different usages (particle/conjunction uses)
  // Note: ながら can be used with nouns in set expressions like 生まれながら、
  // but those are typically covered by other grammar points
  '生まれながらの音楽家だ。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
