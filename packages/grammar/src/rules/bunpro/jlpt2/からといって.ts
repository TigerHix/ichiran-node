import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からといって (karatotte) - Just because, Even though
 *
 * A construction meaning "just because (A) doesn't mean (B)" or "even though (A)".
 * Often used to criticize or give strong opinions. The expression implies that
 * from what is said about (A), (B) is not necessarily true or good.
 *
 * Structures:
 * - Verb［plain form］+ からといって
 * - ［い］Adjective + からといって
 * - ［な］Adjective + だ + からといって
 * - Noun + だ + からといって
 *
 * Variations (abbreviated forms):
 * - からとて (karatote) - literary form
 * - からって (karatte) - colloquial form
 *
 * Examples:
 * - 安いからといって、品質が悪いとは限らない。
 *   (Just because it's cheap doesn't mean the quality is bad.)
 * - 日本人だからといって、日本語ができるとは限らない。
 *   (Just because someone is Japanese doesn't mean they can speak Japanese.)
 * - 暑いからって、そんなに休憩ばかりしていたら仕事が進まないだろ。
 *   (Just because it's hot, if you keep taking breaks like that, work won't progress.)
 *
 * Key discriminators:
 * - からといって follows verbs, i-adjectives, or nouns/na-adjectives+だ
 * - Often used with negative conclusions (とは限らない, わけではない, etc.)
 * - Used for criticism or strong opinions
 * - Can be abbreviated as からとて or からって
 * - Different from から alone (reason marker) or からして (from the fact that)
 *
 * GiNZA parse structure:
 * - 安い(ADJ) + から(SCONJ) + と(ADP) + いっ(VERB) + て(SCONJ) [4 tokens!]
 * - 日本人(NOUN) + だ(AUX) + から(SCONJ) + と(ADP) + いっ(VERB) + て(SCONJ)
 * - 暑い(ADJ) + から(SCONJ) + って(PART) [2 tokens]
 * - Various dependency relations (mark, advcl, fixed)
 */
export default linguisticRule('からといって', (r) => {
  r.either(
    // Branch 1: Full form からといって - から + と + いっ + て (4 tokens)
    (b) => {
      const kara = b.tok({ text: 'から' }, 'kara');
      const to = b.tok({ text: 'と' }, 'to');

      // Constrain kara and to to be adjacent
      b.inOrder(kara, to, 2);

      const itte = b.tok({
        textOneOf: ['いっ', 'イッ', '言っ', '云っ'],
      }, 'itte');

      // Constrain to and itte to be adjacent (kara-to-itte chain)
      b.inOrder(to, itte, 2);

      const te = b.tok({ text: 'て' }, 'te');

      // Constrain itte and te to be adjacent (full chain: kara-to-itte-te)
      b.inOrder(itte, te, 2);

      // Finally, verify all four are within a reasonable span
      b.inOrder(kara, te, 5);

      b.captureSpan('からといって', kara, te);
    },

    // Branch 2: Abbreviated form からって - から + って (2 tokens)
    (b) => {
      const kara = b.tok({ text: 'から' }, 'kara');
      const tte = b.tok({
        textOneOf: ['って', 'ッテ'],
      }, 'tte');

      b.inOrder(kara, tte, 2);

      b.captureSpan('からといって', kara, tte);
    },

    // Branch 3: Abbreviated form からとて - から + とて (2 tokens, literary)
    (b) => {
      const kara = b.tok({ text: 'から' }, 'kara');
      const tote = b.tok({ text: 'とて' }, 'tote');

      b.inOrder(kara, tote, 2);

      b.captureSpan('からといって', kara, tote);
    }
  );
});
