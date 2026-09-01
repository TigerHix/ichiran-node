import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: でしかない (de shika nai) - is nothing but, is only, merely
 *
 * A grammar pattern emphasizing limitation - "nothing but X" or "only X".
 * It expresses that (A) is nothing more than (A), or just (A), with a
 * slightly negative or limiting nuance.
 *
 * Structure:
 * - Noun + で + しか + ない
 * - Noun + で + しか + なかった (past tense)
 *
 * The で here is the te-form of the copula だ (or です).
 * しか is a particle meaning "only" (used with negative)
 * ない is the negative of ある (to exist)
 *
 * Examples:
 * - それは言い訳でしかない。
 *   (That is nothing but an excuse.)
 * - 車は車でしかないよ。
 *   (A car is nothing but a car.)
 * - 賞味期限は目安でしかない。
 *   (The best by date is merely an estimate.)
 * - ウソでしかなかった。
 *   (It was nothing but lies.)
 *
 * Key discriminators:
 * - Must end with で + しか + ない/なかった pattern
 * - で acts as copula te-form (not instrumental/locative)
 * - しか is the restriction particle
 * - ない/なかった is the i-adjective (not verb endings like できない)
 *
 * Different from:
 * - でない (de nai) - simple negation with copula
 * - しかない (shika nai) - "only X" without the copula
 * - にすぎない (ni suginai) - "no more than" (more neutral)
 * - にはかならない (ni hoka naranai) - "nothing but" (very formal)
 * - でしかできない (de shika dekinai) - "can only do with" (potential verb)
 *
 * GiNZA parse structure:
 * - Noun/Verb as base
 * - で as AUX (助動詞, lemma=だ)
 * - しか as ADP (助詞-副助詞, lemma=しか)
 * - ない as AUX (形容詞-非自立可能, lemma=ない)
 * - なかった is split into なかっ (AUX) + た (AUX)
 */
export default linguisticRule('でしかない', (r) => {
  r.either(
    // Pattern 1: Noun + で (AUX) + しか (ADP) + ない (AUX)
    // Standard pattern: e.g., 理想でしかない, 甘えでしかない
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const de = b1.tok({ pos: 'AUX', text: 'で' }, 'de');
      const shika = b1.tok({ pos: 'ADP', text: 'しか' }, 'shika');
      const nai = b1.tok({ pos: 'AUX', text: 'ない' }, 'nai');

      b1.inOrder(noun, de, 2);
      b1.inOrder(de, shika, 1);
      b1.inOrder(shika, nai, 1);

      b1.captureSpan('でしかない', noun, nai);
    },

    // Pattern 2: Noun + で (AUX) + しか (ADP) + なかった
    // Past tense: e.g., ウソでしかなかった
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const de = b2.tok({ pos: 'AUX', text: 'で' }, 'de');
      const shika = b2.tok({ pos: 'ADP', text: 'しか' }, 'shika');
      const nakat = b2.tok({ pos: 'AUX', text: 'なかっ' }, 'nakat');
      const ta = b2.tok({ pos: 'AUX', text: 'た' }, 'ta');

      b2.inOrder(noun, de, 2);
      b2.inOrder(de, shika, 1);
      b2.inOrder(shika, nakat, 1);
      b2.inOrder(nakat, ta, 1);

      b2.captureSpan('でしかない', noun, ta);
    },

    // Pattern 3: Permissive - noun followed by で + しか + nai (any form)
    // Handles POS variations in GiNZA tokenization
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const de = b3.tok({ text: 'で' }, 'de');
      const shika = b3.tok({ text: 'しか' }, 'shika');
      const nai = b3.tok({ textOneOf: ['ない', 'なかっ'] }, 'nai');

      b3.inOrder(noun, de, 3);
      b3.inOrder(de, shika, 2);
      b3.inOrder(shika, nai, 1);

      b3.captureSpan('でしかない', noun, nai);
    },

    // Pattern 4: Combined tokens (when GiNZA doesn't split)
    // e.g., でしかない as a single token or で + しかない
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const deshikanai = b4.tok({
        textOneOf: ['でしかない', 'でしかなかった', 'でしか', 'しかない', 'しかなかった']
      }, 'deshikanai');

      b4.inOrder(noun, deshikanai, 3);
      b4.captureSpan('でしかない', noun, deshikanai);
    }
  );
});
