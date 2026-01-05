import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: いきなり (ikinari) - "suddenly, all of a sudden, for no reason"
 *
 * An adverb indicating that something happens suddenly, abruptly, or without
 * any prior warning. Emphasizes the unexpected and immediate nature of an action
 * or event, often with a sense of surprise or lack of preparation.
 *
 * Structures:
 * - いきなり + Verb/Phrase (modifies following predicate)
 * - 行き成り + Verb/Phrase (kanji form)
 *
 * Examples:
 * - いきなり電話してごめん。
 *   (Sorry for calling you suddenly.)
 * - いきなり雨が降ってきたからびしょびしょだよ。
 *   (It suddenly started raining and I'm drenched.)
 * - ペットのワンちゃんがいきなり吠え出したからびっくりして起きた。
 *   (I was startled awake when my pet dog suddenly started barking.)
 *
 * Key discriminators:
 * - Can be written as いきなり (hiragana) or 行き成り (kanji)
 * - Always functions as an adverb (ADV)
 * - Modifies the following verb or entire phrase
 * - Emphasizes suddenness, lack of warning, and unexpected action
 * - Different from たちまち (focuses on speed/immediacy)
 * - Different from 急に/突然 (general suddenness, less emphasis on "no warning")
 *
 * GiNZA parse structure:
 * - いきなり/行き成り (ADV) - adverb
 * - Followed by predicate (verb) or modifies entire clause
 *
 * Different from:
 * - たちまち (tachimachi) - "immediately, instantly" (emphasizes speed)
 * - 急に (kyuu ni) - "suddenly" (general suddenness)
 * - 突然 (totuzen) - "suddenly, unexpectedly" (general unexpectedness)
 * - いつの間にか (itsu no ma ni ka) - "unnoticed, before realizing" (gradual/unnoticed)
 */
export default linguisticRule('いきなり', (r) => {
  // いきなり/行き成り (suddenly, all of a sudden, for no reason)
  // Adverb meaning something happens abruptly without warning
  // Can be written in hiragana (いきなり) or kanji (行き成り)
  // GiNZA tags this as ADV (adverb)
  // Note: GiNZA uses lemma "いきなり" for both forms

  const ikinari = r.tok({
    lemma: 'いきなり',
    pos: 'ADV',
  }, 'ikinari');

  // Capture just the adverb itself
  // It modifies whatever follows in the sentence
  r.capture(ikinari);
});
