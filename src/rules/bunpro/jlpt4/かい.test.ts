import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かい.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Regular か question particle (not masculine かい)
  'これはペンですか。',
  'あなたのアパートは大きいですか。',
  '大丈夫か？',
  // かい meaning "times" (counter)
  '三回かい四回かいやった。',
  '何回かい行きましたか。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Sentence-final particle かい after explanatory ん (short for の)
//
// GiNZA parses this pattern inconsistently:
//   お前も一緒に行きたいんかい？ → ん + か + い (3 tokens) ✓ WORKS
//   大学生なんかい。 → なんかい (1 token, NOUN) ✗ INDISTINGUISHABLE
//
// The sentence "大学生なんかい。" means "Are you a college student?"
// where "なん" = "なの" (explanatory) + "かい" (masculine question particle).
// However, GiNZA tokenizes "なんかい" as a single NOUN with lemma="なんかい" and reading="ナンカイ",
// which is identical to the counter word "how many times" (何回).
//
// There is no reliable discriminator because:
// 1. The token is a single NOUN (not separate particles)
// 2. lemma="なんかい" could be either the grammar point or the counter
// 3. Matching all "なんかい" NOUNs would cause overcapture on counter usage
//
// CONCLUSION: No reliable discriminator. GiNZA tokenization limitation.
const skipPositives = [
  '大学生なんかい。中学生だと思った。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
