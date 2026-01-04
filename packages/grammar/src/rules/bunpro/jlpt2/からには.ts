import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からには・からは (karaniwa/karaniha) - "Given that, Since, As long as"
 *
 * A structure indicating that from within the general situation of (A),
 * a natural consequence or determination (B) follows. Used when the speaker
 * wants to emphasize that once (A) is true, (B) is unavoidable or the most logical outcome.
 *
 * Structure:
 * - Verb [dictionary form] + からには (e.g., 行くからには, 来たからには)
 * - Verb [past form] + からには (e.g., 買ったからには, なったからには)
 * - Verb [dictionary form] + からは (e.g., 住むからは)
 * - Noun + である + からは (e.g., 教師であるからは)
 *
 * Examples:
 * - 日本に行くからには、たくさんの日本人と話したい。
 *   (As long as I'm going to Japan, I want to speak to many Japanese people.)
 * - 高いスマホを買ったからには、様々な機能を使いこなさなければならない。
 *   (Given that I bought an expensive smartphone, I need to make the most of its various features.)
 * - 日本に住むからは、日本語を勉強するべきだ。
 *   (As long as I am living in Japan, I must study Japanese.)
 * - 教師であるからは、生徒の手本となるべし。
 *   (As long as you are a teacher, you must be an example to your students.)
 *
 * Key discriminators:
 * - Follows verb in dictionary/past form or noun + である
 * - から is a particle (ADP/SCONJ) indicating "from/since"
 * - に is a particle (ADP) followed by は (ADP) for emphasis
 * - Expresses determination or logical consequence
 * - Different from simple から (because) - more emphatic
 *
 * GiNZA parse structure:
 * - Verb + から(ADP/SCONJ) + に(ADP) + は(ADP)
 * - Noun + である(AUX) + から(ADP/SCONJ) + に(ADP) + は(ADP)
 *
 * Different from:
 * - から alone (simple "because")
 * - からして (judging from, based on)
 * - からすると・からすれば (more objective judgment)
 * - 以上 (more formal "now that")
 */
export default linguisticRule('からには', (r) => {
  // Particle から (from/since)
  const kara = r.particle('から', 'kara');

  r.either(
    // Pattern 1: Verb + からには (most common)
    // Verb in any form (dictionary, past, etc.) + から + に + は
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const ni = b1.particle('に', 'ni');
      const wa = b1.particle('は', 'wa');

      b1.inOrder(verb, kara, 3);
      b1.inOrder(kara, ni, 1);
      b1.inOrder(ni, wa, 1);

      b1.captureSpan('からには', verb, wa);
    },

    // Pattern 2: Verb + からは (alternative form, more formal)
    // Verb + から + は
    (b2) => {
      const verb = b2.verb({}, 'verb');
      const wa = b2.particle('は', 'wa');

      b2.inOrder(verb, kara, 2);
      b2.inOrder(kara, wa, 1);

      b2.captureSpan('からには', verb, wa);
    },

    // Pattern 3: Noun + である + からは
    // Noun + copula + から + は
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dearu = b3.aux({ lemma: 'である' }, 'dearu');
      const wa = b3.particle('は', 'wa');

      b3.inOrder(noun, dearu, 1);
      b3.inOrder(dearu, kara, 1);
      b3.inOrder(kara, wa, 1);

      b3.captureSpan('からには', noun, wa);
    }
  );
});
