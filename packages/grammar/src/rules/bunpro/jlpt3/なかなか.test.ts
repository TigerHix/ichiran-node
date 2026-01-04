import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なかなか.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // かなり (similar but different word)
  'かなり大変です。',
  'かなり美味しい。',

  // けっこう (similar but different word)
  'けっこう大変です。',
  'けっこう美味しい。',

  // すごく, とても (similar but different words)
  'すごく大変です。',
  'とても大変です。',
];

// Note: なかなか～ない (negative pattern: "not easily/hardly") is a SEPARATE
// grammar point from this affirmative なかなか usage. Examples like:
//   "なかなか終わらない。" (won't end easily)
//   "なかなか眠れない。" (can hardly sleep)
// are handled by a different rule and are NOT included here.

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
