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
 * - で as ADP (copula form) or AUX
 * - しか as ADP or PART
 * - ない/なかった as ADJ
 */
export default linguisticRule('でしかない', (r) => {
  r.either(
    // Pattern 1: Ultra-flexible - matches noun/verb followed by any form of でしかない
    // Handles various GiNZA tokenizations:
    // - 理想 + で + しか + ない (4 tokens)
    // - 理想 + で + しかない (3 tokens)
    // - 理想 + でしかない (2 tokens)
    // - 理想でしかない (1 token - less likely but possible)
    (b1) => {
      const base = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB'] }, 'base');

      // Match the rest of the pattern in various forms
      // MUST include both で and しか to distinguish from:
      // - ではない (simple negation)
      // - しかない (only X, without copula)
      // - でしかできない (can only do with, potential verb)
      const rest = b1.tok({
        textOneOf: [
          // Full patterns (most reliable)
          'でしかない', 'でしかなかった',
          // Split patterns (でしか combined)
          'でしか',
          // Combined しかない after で
          'しかない', 'しかなかった'
        ]
      }, 'rest');

      // Base and rest must be within 5 tokens of each other
      b1.inOrder(base, rest, 5);

      // Capture from base to rest
      b1.captureSpan('でしかない', base, rest);
    }
  );
});
