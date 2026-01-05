import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: それとも (soretomo) - "or, or else"
 *
 * A conjunction used to present a choice between two alternatives (A) and (B).
 * It specifically emphasizes that the choice is limited to only those two options.
 * Literally meaning "together with that (A), (B)".
 *
 * Structures:
 * - Option A + それとも + Option B (in questions)
 * - [Statement A]、それとも [Statement B]
 * - それとも、[Statement B] (at beginning)
 *
 * Examples:
 * - 今日は文法の勉強をしますか。それとも漢字の勉強をしますか。
 *   (Do you want to study grammar today? Or rather, do you want to study Kanji?)
 * - クレジットカードで払う？それとも現金で？
 *   (Are you paying with a credit card? Or rather, in cash?)
 * - 苦情を言うべきか、それとも我慢すべきか。
 *   (Should we complain? Or rather, should we persevere?)
 *
 * Key characteristics:
 * - Used primarily in questions
 * - Presents exactly two alternatives
 * - Often appears after commas or at sentence boundaries
 * - Similar to か (or) but used at clause/sentence level
 *
 * GiNZA parse structure:
 * - それとも (CCONJ) - conjunction
 * - dep=cc or dep=discourse (conjunction usage)
 *
 * Different from similar conjunctions:
 * - か (or) - directly follows nouns within clauses (今日か明日)
 * - または (matawa) - "or" (more formal,书面)
 * - もしくは (moshikuwa) - "or" (even more formal, legal documents)
 * - あるいは (aruiwa) - "or" (formal, written)
 * - それか (soreka) - "or that, or" (less formal)
 *
 * The rule matches それとも as a standalone conjunction without requiring
 * specific surrounding context since it's grammatically valid in various
 * positions (after comma, at sentence start, between clauses).
 */
export default linguisticRule('それとも', (r) => {
  // それとも is a conjunction presenting alternative choices
  // Used primarily in questions between two options
  //
  // GiNZA typically parses それとも as:
  // - CCONJ (conjunction) or ADV (adverb)
  // - lemma = それとも
  // - dep = cc (coordination) or discourse
  //
  // The rule matches the word in all its positions:
  // - After comma: [A]、それとも[B]
  // - At sentence start: それとも、[B]
  // - Between clauses: [A]？それとも[B]？
  //
  // No additional structural constraints needed since それとも is a
  // standalone conjunction that doesn't depend on surrounding context
  // for grammatical validity.

  const soretomo = r.tok({
    lemma: 'それとも',
    posOneOf: ['CCONJ', 'ADV', 'SCONJ'],
  }, 'soretomo');

  r.capture(soretomo);
});
