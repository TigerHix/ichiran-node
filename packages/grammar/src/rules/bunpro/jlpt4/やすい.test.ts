import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './やすい.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // NOTE: Unfortunately, we cannot programmatically distinguish between:
  // - 易い (easy to do) - attaches to verb stems
  // - 安い (cheap) - independent adjective
  // Both have the same hiragana "やすい" and same lemma in GiNZA.
  //
  // The rule requires やすい to be attached to or near a verb (Branches 1-4, 6),
  // but standalone ADJ tokens (Branch 5) can't distinguish the two meanings.
  //
  // As a result, "やすい" modifying a noun (e.g., "やすいゲーム" = cheap game)
  // will match when parsed as a single ADJ token. This is a known limitation.

  // Verb with different suffixes (にくい, がたい, etc.)
  'この本は読みにくい。', // This book is hard to read (にくい, not やすい)
  '彼は信じがたい。', // He is hard to believe (がたい, not やすい)
  'これは食べにくいです。', // This is hard to eat (opposite)
  '使いにくい道具です。', // Hard-to-use tool (opposite)

  // Verb without やすい suffix
  'この魚は食べる。', // Just "eat this fish" (no やすい)
  'ケーキを作る。', // Just "make cake" (no やすい)

  // Verb in te-form without やすい
  '本を読んでいます。', // I am reading a book

  // Verb in past form without やすい
  '昨日ご飯を食べた。', // I ate rice yesterday

  // やすい by itself (rare, usually used with verbs)
  // Simple adjectives like 簡単な are used instead for "easy"
  '簡単なゲームが好きです。', // I like easy games (易い wouldn't be used here)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
