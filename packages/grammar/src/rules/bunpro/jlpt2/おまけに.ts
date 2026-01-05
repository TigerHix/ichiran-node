import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: おまけに (omake ni) - "on top of that, to make matters worse, in addition"
 *
 * A casual conjunction that adds additional information, often with negative
 * or emphatic nuance. Literally comes from "おまけ" (freebie/bonus) + "に" (particle),
 * meaning something like "as an extra on top of that."
 *
 * Unlike neutral conjunctions like "その上" or "それに", おまけに often carries
 * a stronger emotional tone - either emphasizing how bad a situation is becoming
 * ("to make matters worse") or expressing surprise at an unexpected bonus.
 *
 * Structures:
 * - [Statement A]. おまけに、[Statement B].
 * - [Statement A]。おまけに、[Statement B].
 * - [Statement A]、おまけに [Statement B].
 *
 * Examples:
 * - 自転車が盗まれただけではなく、おまけに雨が降りそうだ。
 *   (Not only has my bike been stolen, but to make matters worse, it looks like it's going to rain.)
 * - 競馬で負け、おまけに帰りに財布を落とし、最悪だ。
 *   (I lost at the horse races and, to make matters worse, I lost my wallet on the way home. Worst day ever.)
 * - 家を掃除した。おまけにランチを作っといた。
 *   (I cleaned the house. On top of that, I also made lunch in advance.)
 *
 * Key characteristics:
 * - Casual/colloquial register (話し言葉)
 * - Often used with negative situations ("to make matters worse")
 * - Can also be used positively for unexpected bonuses
 * - Emphasizes that (B) is an "extra" addition to (A)
 * - (A) and (B) are always same polarity (both good or both bad)
 *
 * Kanji variants:
 * - おまけに (standard hiragana)
 * - 御負けに (rare kanji form)
 *
 * GiNZA parse structure:
 * - おまけ (NOUN) - "freebie, bonus, discount"
 * - に (ADP) - particle
 * - Functions as compound ADV/CONJ
 * - dep=advmod or dep=discourse
 *
 * Different from similar conjunctions:
 * - その上 (sono ue) - "moreover" (formal, neutral)
 * - それに (sore ni) - "and besides" (neutral, simple addition)
 * - しかも (shikamo) - "moreover" (neutral, emphatic)
 * - さらに (sara ni) - "furthermore" (progression/escalation)
 * - 上に (ue ni) - "in addition to" (prepositional, within sentence)
 */
export default bunproLinguisticRule('おまけに', (r) => {
  // おまけに is a casual conjunction meaning "on top of that, to make matters worse"
  // It connects clauses by adding additional (often worse) information
  //
  // GiNZA typically parses おまけに as:
  // - Often tokenized as two separate tokens: おまけ + に
  // - Sometimes as a single compound token
  // - Forms: おまけに (hiragana), 御負けに (rare kanji)
  //
  // The key is that it functions as a conjunction/adverb that introduces
  // additional information, often with negative or emphatic nuance.
  //
  // Common forms:
  // - おまけに (standard hiragana form)
  // - 御負けに (rare kanji form, usually おまけに in practice)

  r.either(
    // Pattern 1: Single token おまけに/御負けに
    (b1) => {
      const omake = b1.tok({
        textOneOf: ['おまけに', '御負けに'],
      }, 'omake');
      b1.capture(omake);
    },

    // Pattern 2: Two tokens: おまけ + に
    (b2) => {
      const omakeNoun = b2.tok({
        textOneOf: ['おまけ', '御負け'],
        pos: 'NOUN',
      }, 'omake');
      const ni = b2.particle('に', 'ni');

      b2.inOrder(omakeNoun, ni, 1);  // Must be adjacent
      b2.captureSpan('おまけに', omakeNoun, ni);
    }
  );
});
