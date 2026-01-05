import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からには (kara ni wa) - "Now that, Since, Given that"
 *
 * An expression used when something is either obvious or determined upon based
 * on the previous information. Translates as "as long as (A), (B)", "since (A), (B)",
 * or "given that (A), (B)".
 *
 * Structure:
 * - Verb (dictionary form or ta-form) + からには
 * - I-adjective + からには
 * - Na-adjective + である + からには
 * - Noun + である + からには
 *
 * Examples:
 * - 沖縄に来たからには、一番綺麗なビーチへ行きたい。
 *   (Since I came to Okinawa, I want to go to the prettiest beach.)
 * - 高いスマホを買ったからには、様々な機能を使いこなさなければならない。
 *   (Given that I bought an expensive smartphone, I need to make the most of its features.)
 * - 二十歳になるからには、ドライブしに行こう！
 *   (Now that I'll be turning 20, let's go for a drive!)
 * - 日本に住むからは、日本語を勉強するべきだ。
 *   (As long as I am living in Japan, I must study Japanese.)
 *
 * Key discriminators:
 * - Expresses determination or necessity based on a condition
 * - Stronger sense of personal resolve than simple から
 * - Follows various word forms (verbs, adjectives, nouns + である)
 * - Second part of sentence contains speaker's judgment or intention
 * - Different from simple から (because) which is more neutral
 *
 * GiNZA parse structure:
 * - 来た(VERB) + から(ADP) + に(ADP) + は(PART)
 * - 住む(VERB) + からは(ADP/FIXED)
 *
 * Different from:
 * - Simple から (because) - more neutral, lacks emphasis
 * - からして (judging from, based on) - more subjective
 * - からすると・からすれば (more objective judgment)
 */
export default linguisticRule('からには', (r) => {
  r.either(
    // Pattern 1: Verb/Aux (dictionary form or ta-form) + からには
    (b1) => {
      const verb = b1.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const kara = b1.particle('から', 'kara');
      const ni = b1.particle('に', 'ni');
      const wa = b1.particle('は', 'wa');

      b1.inOrder(verb, kara, 5);
      b1.inOrder(kara, ni, 1);
      b1.inOrder(ni, wa, 1);

      b1.captureSpan('からには', verb, wa);
    },

    // Pattern 2: I-adjective + からには
    (b2) => {
      const adj = b2.adj({}, 'adj');
      const kara = b2.particle('から', 'kara');
      const ni = b2.particle('に', 'ni');
      const wa = b2.particle('は', 'wa');

      b2.inOrder(adj, kara, 5);
      b2.inOrder(kara, ni, 1);
      b2.inOrder(ni, wa, 1);

      b2.captureSpan('からには', adj, wa);
    },

    // Pattern 3: Na-adjective + である + からには
    (b3) => {
      const adj = b3.adj({}, 'adj');
      const dearu = b3.aux({ lemma: 'だ' }, 'dearu');
      const kara = b3.particle('から', 'kara');
      const ni = b3.particle('に', 'ni');
      const wa = b3.particle('は', 'wa');

      b3.inOrder(adj, dearu, 3);
      b3.inOrder(dearu, kara, 3);
      b3.inOrder(kara, ni, 1);
      b3.inOrder(ni, wa, 1);

      b3.captureSpan('からには', adj, wa);
    },

    // Pattern 4: Noun + である + からには
    (b4) => {
      const noun = b4.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const dearu = b4.aux({ lemma: 'だ' }, 'dearu');
      const kara = b4.particle('から', 'kara');
      const ni = b4.particle('に', 'ni');
      const wa = b4.particle('は', 'wa');

      b4.inOrder(noun, dearu, 3);
      b4.inOrder(dearu, kara, 3);
      b4.inOrder(kara, ni, 1);
      b4.inOrder(ni, wa, 1);

      b4.captureSpan('からには', noun, wa);
    },

    // Pattern 5: からは variant (more formal, sometimes in writing)
    (b5) => {
      const verb = b5.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const karaha = b5.tok({ text: 'からは' }, 'karaha');

      b5.inOrder(verb, karaha, 5);

      b5.captureSpan('からには', verb, karaha);
    },

    // Pattern 6: Noun + である + からは
    (b6) => {
      const noun = b6.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const dearu = b6.aux({ lemma: 'だ' }, 'dearu');
      const karaha = b6.tok({ text: 'からは' }, 'karaha');

      b6.inOrder(noun, dearu, 3);
      b6.inOrder(dearu, karaha, 3);

      b6.captureSpan('からには', noun, karaha);
    }
  );
});
