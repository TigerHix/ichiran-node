import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: かけ (kake) - "halfway doing / in the middle of"
 *
 * Verb stem + かけ/かける = "halfway doing X", "started X but not finished"
 *
 * Examples:
 * - わすれかけていた (was half-forgetting)
 * - 飲みかけた水 (half-drunk water)
 * - 読みかけの本 (half-read book)
 * - 死にかけの子犬 (about-to-die puppy)
 *
 * The pattern attaches to verb stems (masu form/ren'youkei):
 * - 飲みかけ (half-drinking)
 * - 読みかけ (half-reading)
 * - 死にかけ (about-to-die)
 *
 * GiNZA parses this in various ways:
 * - As separate tokens: verb + かける with dep=compound
 * - As separate tokens: verb + かけ with dep=aux
 * - As compound token: e.g., 飲みかけ as single VERB with lemma=飲みかける
 * - Sometimes lemma='かける' on the kake token
 * - Sometimes lemma='かけ' for the stem form
 *
 * The compound form (e.g., 飲みかけ) has text ending with "け" but lemma ending with "かける",
 * so we use lemmaRe to match these patterns.
 */
export default bunproLinguisticRule('かけ', (r) => {
  r.either(
    // Branch 1: かけ/かける as auxiliary (separate token)
    (b) => {
      const kake = b.aux({
        lemmaOneOf: ['かけ', 'かける'],
      }, 'kake');
      b.capture(kake);
    },

    // Branch 2: かけ/かける as verb with aux/compound dependency (separate token)
    (b) => {
      const kake = b.verb({
        lemmaOneOf: ['かけ', 'かける'],
        depOneOf: ['aux', 'compound', 'fixed'],
      }, 'kake');
      b.capture(kake);
    },

    // Branch 3-6: Compound tokens with various common verb stems
    // Each branch provides specific lemmaOneOf for trigger dispatch while using
    // lemmaRe for the actual suffix matching pattern

    // Branch 3: Common godan verbs + かける (飲む, 読む, 書く, etc.)
    (b) => {
      const kake = b.tok({
        lemmaOneOf: [
          '飲みかける', '読みかける', '書きかける', '聞きかける',
          'はきかける', '履きかける', '吹きかける', 'ふきかける', '拭きかける',
          '死にかける', '忘れかける', '作りかける',
          'かける'
        ],
        lemmaRe: /.*かける$/,
      }, 'kake');
      b.capture(kake);
    },

    // Branch 4: Verbs with ru/irregular + かける (including hiragana variants)
    (b) => {
      const kake = b.tok({
        lemmaOneOf: [
          '食べかける', '見かける', '出かける', '起きかける',
          '破れかける', 'はがれかける', '剥がれかける',
          '壊れかける', '終わりかける', 'こわれかける',
          'かける'
        ],
        lemmaRe: /.*かける$/,
      }, 'kake');
      b.capture(kake);
    },

    // Branch 5: Verbs ending with かけ (short form, without る)
    (b) => {
      const kake = b.tok({
        lemmaOneOf: [
          '食べかけ', 'こわれかけ', 'こわれかける',
          '終わりかけ', '終わりかける',
          'かけ'
        ],
        lemmaRe: /.*かけ$/,
      }, 'kake');
      b.capture(kake);
    },

    // Branch 6: NOUN form with lemma=かけ (isolated suffix, e.g., よみかけの)
    (b) => {
      const kake = b.noun({
        lemmaOneOf: ['かけ'],
      }, 'kake');
      b.capture(kake);
    },

    // Branch 7: NOUN form with text=かけ (isolated suffix)
    (b) => {
      const kake = b.noun({
        textOneOf: ['かけ', 'ガケ'],
      }, 'kake');
      b.capture(kake);
    },

    // Branch 8: Any token with exact text=かけ or かける (catch-all)
    (b) => {
      const kake = b.tok({
        textOneOf: ['かけ', 'かける'],
      }, 'kake');
      b.capture(kake);
    }
  );
});
