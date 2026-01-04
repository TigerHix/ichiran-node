import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './どんなに-ても.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // どんな as question "what kind of" (not "no matter what kind")
  'どんな本が好きですか。',
  'どんな色がありますか。',
  'どんな人が来ましたか。',

  // どんなに as question "how much" (not "no matter how much")
  'どんなに欲しいですか。',
  'どんなに必要ですか。',

  // どんな + noun without でも (simple modification)
  'どんな服を着ていますか。',
  'どんな音楽が好きですか。',

  // Related but different grammar: いくら-でも (focuses on amount/quantity)
  // This would be matched by the いくら-でも rule instead
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
