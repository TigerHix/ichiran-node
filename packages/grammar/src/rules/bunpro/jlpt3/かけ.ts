import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: かけ (kake) - "halfway doing / in the middle of"
 *
 * Verb stem + かけ/かける = "halfway doing X", "started X but not finished"
 *
 * Examples:
 * - わすれかけていた (was half-forgetting)
 * - のみかけのジュース (half-drunk juice)
 * - 読みかけの本 (half-read book)
 * - 死にかけの子犬 (about-to-die puppy)
 *
 * The pattern attaches to verb stems (masu form/ren'youkei):
 * - 飲みかけ (half-drinking)
 * - 読みかけ (half-reading)
 * - 死にかけ (about-to-die)
 *
 * GiNZA parses this in various ways:
 * - As compound: verb + かける with dep=compound
 * - As aux: verb + かけ with dep=aux
 * - Sometimes lemma='かける' on the kake token
 * - Sometimes lemma='かけ' for the stem form
 * - Sometimes as NOUN when used as a nominal suffix (のみかけの)
 */
export default linguisticRule('かけ', (r) => {
  r.either(
    // Branch 1: かけ/かける as auxiliary (dep=aux or dep=compound)
    (b) => {
      const kake = b.aux({
        lemmaOneOf: ['かけ', 'かける'],
      }, 'kake');
      b.capture(kake);
    },

    // Branch 2: かけ/かける as verb with aux/compound dependency
    (b) => {
      const kake = b.verb({
        lemmaOneOf: ['かけ', 'かける'],
        depOneOf: ['aux', 'compound', 'fixed'],
      }, 'kake');
      b.capture(kake);
    },

    // Branch 3: かけ as NOUN (when used as nominal suffix like のみかけの)
    (b) => {
      const kake = b.noun({
        textOneOf: ['かけ', 'ガケ'],
      }, 'kake');
      b.capture(kake);
    },

    // Branch 4: かける as VERB/AUX with any pos
    (b) => {
      const kake = b.tok({
        textOneOf: ['かけ', 'かける'],
      }, 'kake');
      b.capture(kake);
    }
  );
});
