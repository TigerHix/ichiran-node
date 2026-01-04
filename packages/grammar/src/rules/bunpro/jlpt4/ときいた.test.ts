import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ときいた.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases - sentences that should NOT match
const negatives = [
  // Direct quotation with 言う instead of 聞く
  // "彼は「危ない」と言った。" - He said "It's dangerous."
  // This uses 言った, not 聞いた

  // Thinking/Opinion: とおもう (I think)
  // "彼は来るとおもう。" - I think he will come.
  // Uses 思う instead of 聞く

  // Hearsay: そうだ / そうです (I heard that)
  // "彼は来るそうだ。" - I heard he is coming.
  // Uses そうだ instead of ときいた

  // Apparent/Seems: らしい (apparently/seems)
  // "彼は来るらしい。" - Apparently he is coming.

  // Physical hearing: 聞こえる (can hear/is audible)
  // "音が聞こえる。" - I can hear the sound.
  // Different verb meaning (physical perception vs hearsay)

  // Different grammar: という (called/named)
  // "ポケモンというゲーム" - A game called Pokemon

  // Different grammar: といわれている (it is said that)
  // "危ないと言われている" - It is said that it's dangerous
  // Uses passive progressive form instead of simple past

  // Quoting with って but without 聞く
  // "彼だって来るって。" - Even he is coming, they say.
  // Different use of って (emphatic particle)

  // Question: と聞く (to ask)
  // "道と聞く。" - Ask about the way.
  // 聞く meaning "to ask" not "heard"

  // Quoting what someone said: と言う (to say that)
  // "彼は行くと言った。" - He said he would go.
  // Uses 言う instead of 聞く

  // Quoting with quotation marks
  // "「来る」と言った。" - He said "I'm coming."

  // 聨かない (don't hear) - negative form
  // "音が聞こえない。" - I can't hear the sound.

  // 聨いて (listening) - te-form of 聞く
  // "音楽を聞いています。" - I am listening to music.

  // Just と followed by other verbs
  // "彼を見ると笑った。" - When I saw him, I laughed.
  // と as conditional "when", not quote particle
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
