import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: たちまち (tachimachi) - "immediately, instantly, in a flash"
 *
 * An adverb indicating that something happens suddenly, instantly, or in a very
 * short period of time without thought or control. Expresses spontaneous,
 * uncontrollable rapid change.
 *
 * Structures:
 * - たちまち + Phrase/Verb (modifies following predicate)
 * - 忽ち + Phrase/Verb (kanji form)
 *
 * Examples:
 * - たちまち有名になる。
 *   (To immediately become famous.)
 * - 私と田中さんはたちまち仲良くなった。
 *   (Tanaka-san and I immediately became friends.)
 * - たった一つの不良品で、忽ち売れ行きがストップしてしまうこともある。
 *   (There is a chance that your sales will immediately cease if you have but one faulty product.)
 *
 * Key discriminators:
 * - Can be written as たちまち (hiragana) or 忽ち (kanji)
 * - Always functions as an adverb (ADV)
 * - Modifies the following verb, adjective, or entire phrase
 * - Emphasizes speed, spontaneity, and lack of control
 * - Different from ただちに (controlled urgency) and いきなり (without warning)
 *
 * GiNZA parse structure:
 * - たちまち/忽ち (ADV) - adverb
 * - Followed by predicate (verb, adjective) or modifies entire clause
 *
 * Different from:
 * - ただちに (tadachini) - "immediately" (formal, with control/urgency)
 * - いきなり (ikinari) - "suddenly, abruptly" (emphasizes lack of warning)
 * - すぐに (sugu ni) - "immediately, soon" (neutral, everyday usage)
 * - いつの間にか (itsu no ma ni ka) - "unnoticed, before realizing" (gradual/unnoticed)
 * - はやく (hayaku) - "quickly, early" (general speed)
 */
export default linguisticRule('たちまち', (r) => {
  // たちまち/忽ち (immediately, instantly, in a flash)
  // Adverb meaning something happens spontaneously and quickly without control
  // Can be written in hiragana (たちまち) or kanji (忽ち)
  // GiNZA tags this as ADV (adverb)

  const tachimachi = r.tok({
    lemmaOneOf: ['たちまち', '忽ち'],
    pos: 'ADV',
  }, 'tachimachi');

  // Capture just the adverb itself
  // It modifies whatever follows in the sentence
  r.capture(tachimachi);
});
