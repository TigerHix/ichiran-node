import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: た末・の末 (ta sue - no sue) - After, As a conclusion of
 *
 * Expresses that (B) is the conclusion of (A), emphasizing the completion
 * of (A) and the effort to reach completion.
 *
 * Structures:
 * - Verb［た］+ 末 + (に) + (Result)
 * - Verb［た］+ 末 + の + Noun
 * - Noun + の + 末 + (に) + (Result)
 * - Noun + の + 末 + の + Noun
 *
 * Examples:
 * - 色々と考えた末に、お父さんの会社を継ぐことに決めた。
 *   (After much thought, I decided to take over my father's company.)
 * - ５年間にわたる争いの末、アメリカ軍が撤退を開始した。
 *   (After five years of conflict, the U.S. military began to withdraw.)
 * - 社長と長い議論の末に、人事の伊藤さんをクビにすることに決めた。
 *   (After a long discussion with the president, we decided to fire Ito-san.)
 *
 * Key discriminators:
 * - 末 is a noun (adverbial noun) meaning "end", "conclusion", or "result"
 * - Follows verb ta-form or noun+の
 * - Optional particle に follows 末
 * - GiNZA parses 末 as NOUN or PROPN
 *
 * GiNZA parse structure:
 * - 考えた末: 考え(VERB) + た(AUX) + 末(NOUN)
 * - 争いの末: 争い(NOUN) + の(ADP) + 末(NOUN)
 */
export default bunproLinguisticRule('た末-の末', (r) => {
  r.either(
    // Pattern 1: Verb［た］+ 末
    (b) => {
      const verb = b.verb({}, 'verb');

      // Match ta-form auxiliary (た)
      const ta = b.tok({
        lemma: 'た',
        posOneOf: ['AUX', 'SCONJ'],
      }, 'ta');

      // ta must be attached to verb (either as aux or mark - GiNZA quirk)
      b.either(
        (eb) => {
          eb.auxOf(verb, ta);
        },
        (eb) => {
          eb.headChild(verb, ta, 'mark');
        }
      );

      // Followed by 末 (or すえ in hiragana)
      const sue = b.tok({
        textOneOf: ['末', 'すえ', '末に', 'すえに', '末の', 'すえの'],
        lemmaOneOf: ['末', 'すえ'],
        posOneOf: ['NOUN', 'PROPN'],
      }, 'sue');

      b.inOrder(ta, sue, 5);

      // Optional particle に (if not already part of sue token)
      b.optional((ob) => {
        const ni = ob.tok({ text: 'に' }, 'ni');
        ob.inOrder(sue, ni, 1);
      });

      b.captureSpan('た末-の末', verb, sue);
    },

    // Pattern 1b: Any token with text た + 末 (for compound verbs)
    // This handles cases like "走って転んだ末" where multiple verbs are connected
    (b) => {
      // Match the ta auxiliary
      const ta = b.tok({
        text: 'た',
        posOneOf: ['AUX', 'SCONJ'],
      }, 'ta');

      // Followed by 末 (or すえ in hiragana)
      const sue = b.tok({
        textOneOf: ['末', 'すえ', '末に', 'すえに', '末の', 'すえの'],
        lemmaOneOf: ['末', 'すえ'],
        posOneOf: ['NOUN', 'PROPN'],
      }, 'sue');

      b.inOrder(ta, sue, 15);

      // Optional particle に (if not already part of sue token)
      b.optional((ob) => {
        const ni = ob.tok({ text: 'に' }, 'ni');
        ob.inOrder(sue, ni, 1);
      });

      // Find any verb before ta (for compound verbs, we capture the chain)
      const verb = b.verb({}, 'verb');
      b.inOrder(verb, ta, 20);

      b.captureSpan('た末-の末', verb, sue);
    },

    // Pattern 1c: Verb directly followed by 末/すえ (handles verbs in past form)
    // This catches cases where the past tense is fused with the verb
    (b) => {
      const verb = b.verb({}, 'verb');

      // Followed by 末 (or すえ in hiragana)
      const sue = b.tok({
        textOneOf: ['末', 'すえ', '末に', 'すえに', '末の', 'すえの'],
        lemmaOneOf: ['末', 'すえ'],
        posOneOf: ['NOUN', 'PROPN'],
      }, 'sue');

      b.inOrder(verb, sue, 5);

      // Optional particle に (if not already part of sue token)
      b.optional((ob) => {
        const ni = ob.tok({ text: 'に' }, 'ni');
        ob.inOrder(sue, ni, 1);
      });

      b.captureSpan('た末-の末', verb, sue);
    },

    // Pattern 2: Noun + の + 末
    (b) => {
      const noun = b.noun({}, 'noun');

      // Possessive particle の
      const no = b.tok({
        text: 'の',
        pos: 'ADP',
      }, 'no');

      b.caseMarker(noun, no);

      // Followed by 末 (or すえ in hiragana)
      const sue = b.tok({
        textOneOf: ['末', 'すえ', '末に', 'すえに', '末の', 'すえの'],
        lemmaOneOf: ['末', 'すえ'],
        posOneOf: ['NOUN', 'PROPN'],
      }, 'sue');

      b.inOrder(no, sue, 5);

      // Optional particle に (if not already part of sue token)
      b.optional((ob) => {
        const ni = ob.tok({ text: 'に' }, 'ni');
        ob.inOrder(sue, ni, 1);
      });

      b.captureSpan('た末-の末', noun, sue);
    }
  );
});
