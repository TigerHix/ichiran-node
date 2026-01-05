import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いかなる.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // どんな (casual "what kind of") - different POS and dep
  'どんな人が来ましたか。',
  'どんな色が好きですか。',
  'どんな本を読んでいますか。',

  // どのような (formal variant of どんな) - different structure
  'どのような状況ですか。',
  'どのような意味ですか。',

  // Simple question words that look similar
  '何をしていますか。',
  'どこに行きますか。',

  // いかな (rare variant without る) - different word
  // Note: いかな+noun exists but is very rare, we only match いかなる
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
