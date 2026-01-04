import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './という-called.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences with similar-looking patterns that should NOT match
const negatives = [
<<<<<<< HEAD
  // かというと - "if we ask" (requires question particle か)
=======
  // かというと - "if we ask" (requires question particle か before という)
>>>>>>> jlpt3-という-called
  'なぜ行かなかったかというと、病気だったからです。',
  'どうして遅れたかというと、電車が遅れたんです。',
  // かというか - "or rather" expression
  '彼が来なかったかというか、遅れただけです。',
<<<<<<< HEAD
  // といった - "such as" (listing items, different lemma)
  'りんごやみかんといった果物が好きです。',
  '東京や大阪といった都市が好きです。',
  // というのは - topic marker + nominalizer (followed by explanation, not noun)
  // Note: Some sentences like "人生というのは..." use the same という but with は
  // Our rule correctly catches these but they're a separate grammar point
  // ということ - nominalizer (followed by こと, not regular noun)
  '彼が来るということです。',
  '日本に行くということだ。',
=======
  // といった - "such as" (listing items, different lemma=といった)
  'りんごやみかんといった果物が好きです。',
  '東京や大阪といった都市が好きです。',
  // Simple と quoting (not いう) - should not match
  '彼は「行く」と言った。',
  '「好きだ」と答えた。',
>>>>>>> jlpt3-という-called
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
