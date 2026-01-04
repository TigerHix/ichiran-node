import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: さすが (sasuga) - "as expected, as one would expect"
 *
 * An adverb indicating that something is just as expected, or lives up to
 * its reputation. Often used to express admiration or confirmation of
 * expectations about someone or something.
 *
 * Structures:
 * - さすが（に）+ Phrase
 * - さすが（の）+ Noun
 * - さすが alone (at end of sentence or before noun)
 *
 * Examples:
 * - さすがにプロだ、上手だ。
 *   (As expected of a pro, [he] is skilled.)
 * - さすがの彼も驚いた。
 *   (Even he, as would be expected, was surprised.)
 * - いいカーブだ。さすがダルビッシュ。
 *   (Nice curve. That's just like Darvish.)
 *
 * Key discriminators:
 * - Can be written as さすが (hiragana) or 流石 (kanji)
 * - May be followed by particle に (adverbial) or の (pre-nominal)
 * - May stand alone when modifying a noun directly
 * - Expresses expectation, admiration, or confirmation of reputation
 *
 * GiNZA parse structure:
 * - さすが/流石 (ADV) - adverb
 * - May be followed by:
 *   - に (ADP) - adverbial particle
 *   - の (ADP) - possessive/genitive particle for pre-nominal use
 *   - Direct noun (compound/obl dependency)
 *
 * Different from:
 * - やっぱり/やはり (yappari/yahari) - "as expected/after all" (less emphatic)
 * - はたして (hatashite) - "really, actually" (expresses doubt, not affirmation)
 */
export default linguisticRule('さすが', (r) => {
  r.either(
    // Pattern 1: さすが + に + Phrase (adverbial with particle)
    // Most common pattern: sasuga ni + predicate
    (b1) => {
      const sasuga = b1.tok({
        lemmaOneOf: ['さすが', '流石'],
        pos: 'ADV',
      }, 'sasuga');
      const ni = b1.particle('に', 'ni');

      b1.inOrder(sasuga, ni, 1);
      b1.captureSpan('さすがに', sasuga, ni);
    },

    // Pattern 2: さすが + の + Noun (pre-nominal with particle)
    // Used before nouns: sasuga no + noun
    (b2) => {
      const sasuga = b2.tok({
        lemmaOneOf: ['さすが', '流石'],
        pos: 'ADV',
      }, 'sasuga');
      const no = b2.particle('の', 'no');
      const noun = b2.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');

      b2.inOrder(sasuga, no, 1);
      b2.inOrder(no, noun, 1);
      b2.captureSpan('さすがの', sasuga, noun);
    },

    // Pattern 3: さすが alone at beginning of sentence/phrase
    // Exclamatory use: "Sasuga!" (That's just like him/her!)
    (b3) => {
      const sasuga = b3.tok({
        lemmaOneOf: ['さすが', '流石'],
        pos: 'ADV',
      }, 'sasuga');

      // Capture just the word
      b3.capture(sasuga);
    }
  );
});
