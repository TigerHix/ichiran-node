import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことなく.js';
import { BUNPRO_JLPT2 } from './index.js';

// False positives: sentences that should NOT match ことなく
const negatives = [
  // Simple こと (thing/matter) followed by なく in different contexts
  '何もすることがなく、暇だった。', // Nothing to do + was bored (different grammar: ことがなく = "there is no thing to do")
  'ことなくては生きていけない。', // Cannot live without things (different structure)

  // ないで (without doing) - different grammar point
  '朝ごはんを食べないで学校に行った。', // Went to school without eating breakfast (using ないで)
  '傘を持たないで出かけた。', // Went out without umbrella (using ないで)

  // ずに (without doing) - different grammar point (JLPT3)
  '水を飲まずに運動をしていた。', // Was exercising without drinking water (using ずに)
  '何も知らずにあんなこと言ってごめんなさい。', // Sorry for saying that without knowing (using ずに)

  // こと as a regular noun (not nominalizer)
  'それは大事なことだ。', // That's an important thing (こと = thing/matter)
  '言葉通りことなく進んでいる。', // Proceeding as expected (different usage)

  // なく in different contexts (not negative auxiliary)
  'お金がなくて困っている。', // Have no money and troubled (なくて = "because there is no")
  '気なく言った。', // Said casually (気なく = different word)

  // Other negative forms
  '彼は来なかった。', // He didn't come (simple past negative)
  '雨が降らない。', // It won't rain (simple negative)
];

// Sentences that should match but are skipped due to known limitations
const skipPositives = [
  // None expected - this rule should handle all the test cases
  // The alternative forms (こともなく, ことなしに) are handled by separate branches
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
