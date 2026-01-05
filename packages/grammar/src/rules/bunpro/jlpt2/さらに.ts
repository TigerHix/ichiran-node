import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: さらに (sarani) - "furthermore, moreover, even more, again"
 *
 * An adverb indicating addition, progression, or intensification.
 * Used to express that something continues "further" or "beyond" a
 * certain point, often with a nuance of surprising increase.
 *
 * Can be written as:
 * - さらに (hiragana)
 * - 更に (kanji)
 *
 * Structures:
 * - さらに + Verb/Adjective (modifying predicate)
 * - さらに + Sentence (conjunctive use at sentence beginning)
 *
 * Examples:
 * - 電気代がさらに高くなった。
 *   (The electricity bill has become even more expensive.)
 * - さらに給与が上がり、とても助かります。
 *   (My salary has gone up even more, and that is very helpful.)
 * - スマホを見る時間がさらに増えた。
 *   (The time I spend looking at my smartphone has increased even more.)
 *
 * Key discriminators:
 * - POS is ADV (adverb) or CCONJ (conjunction)
 * - Expresses addition, progression, or intensification
 * - Can appear at the beginning of a clause or sentence
 * - Modifies verbs, adjectives, or entire clauses
 *
 * GiNZA parse structure:
 * - さらに/更に (ADV or CCONJ) - adverb or conjunction
 * - ADV: modifies following predicate with dep=advmod
 * - CCONJ: connects clauses with dep=cc
 *
 * Different from:
 * - また (mata) - "also, again" (less formal, more common)
 * - その上 (sono ue) - "besides, in addition" (conjunction-focused)
 * - しかも (shikamo) - "moreover, furthermore" (emphasizes reinforcement)
 * - ますます (masumasu) - "increasingly, more and more" (state intensification)
 * - 再び (futatabi) - "again, once more" (repetition after pause)
 */
export default linguisticRule('さらに', (r) => {
  r.either(
    // Pattern 1: さらに as adverb (ADV) modifying a predicate
    // Most common pattern: modifies verb or adjective
    // Example: さらに高くなった (became even more expensive)
    (b1) => {
      const sarani = b1.tok({
        lemmaOneOf: ['さらに', '更に'],
        posOneOf: ['ADV', 'CCONJ'],
      }, 'sarani');

      b1.capture(sarani);
    }
  );
});
