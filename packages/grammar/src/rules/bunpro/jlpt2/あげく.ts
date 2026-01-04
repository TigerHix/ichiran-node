import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: あげく (ageku) - After, In the end, After all, Eventually
 *
 * A construction often implying the negative result of (A), despite all efforts.
 * "To have all amounted to nothing when (A) happened."
 *
 * Structures:
 * - Verb［た］+ あげく + (に) + (Past Tense) (Result)
 * - Verb［た］+ あげく + の + Noun
 * - Noun + の + あげく + (に) + (Past Tense) (Result)
 *
 * Examples:
 * - さんざん泣いたあげく、彼女は眠ってしまった。
 *   (After crying a lot, she fell asleep.)
 * - 口論のあげく、喧嘩になってしまい、警察のお世話になった。
 *   (After an argument, they got into a fight and were taken care of by the police.)
 * - 数時間スーパーで歩き回ったあげくに何も買わず家に帰った。
 *   (After walking around at the supermarket for a few hours, I went home without buying anything.)
 *
 * Key discriminators:
 * - あげく is a noun (adverbial noun) meaning "in the end" or "finally"
 * - Follows verb ta-form or noun+の
 * - Optional particle に follows あげく
 * - GiNZA parses あげく as NOUN or PROPN
 *
 * GiNZA parse structure:
 * - 泣いたあげく: 泣い(VERB) + た(AUX) + あげく(NOUN)
 * - 口論のあげく: 口論(NOUN) + の(ADP) + あげく(NOUN)
 */
export default linguisticRule('あげく', (r) => {
  r.either(
    // Pattern 1: Verb［た］+ あげく
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

      // Followed by あげく (NOUN or PROPN)
      // GiNZA may parse as either hiragana あげく or kanji 挙げ句
      // May also parse あげくに as a single adverb
      const ageku = b.tok({
        lemmaOneOf: ['あげく', '挙げく', 'あげくに', '挙げくに'],
        posOneOf: ['NOUN', 'PROPN', 'ADV'],
      }, 'ageku');

      b.inOrder(ta, ageku, 3);

      b.captureSpan('あげく', verb, ageku);
    },

    // Pattern 2: Noun + の + あげく
    (b) => {
      const noun = b.noun({}, 'noun');

      // Possessive particle の
      const no = b.tok({
        text: 'の',
        pos: 'ADP',
      }, 'no');

      b.caseMarker(noun, no);

      // Followed by あげく (may be hiragana or kanji)
      const ageku = b.tok({
        lemmaOneOf: ['あげく', '挙げく'],
        posOneOf: ['NOUN', 'PROPN'],
      }, 'ageku');

      b.inOrder(no, ageku, 3);

      // Optional particle に
      b.optional((ob) => {
        const ni = ob.tok({ text: 'に' }, 'ni');
        ob.inOrder(ageku, ni, 1);
      });

      b.captureSpan('あげく', noun, ageku);
    }
  );
});
