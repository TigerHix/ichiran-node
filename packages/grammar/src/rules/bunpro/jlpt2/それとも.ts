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
 * - Single token: それとも (CCONJ/ADV/SCONJ)
 * - Or multi-token: それ (PRON/CCONJ) + とも (PART/SCONJ/ADP)
 * - dep=cc or dep=discourse (conjunction usage)
 *
 * Different from similar conjunctions:
 * - か (ka) - question particle or "or" within clause (今日か明日)
 * - または (matawa) - "or" (more formal,书面)
 * - もしくは (moshikuwa) - "or" (even more formal, legal documents)
 * - あるいは (aruiwa) - "or" (formal, written)
 * - それか (soreka) - "or that, or" (less formal)
 */
export default linguisticRule('それとも', (r) => {
  // それとも is a conjunction presenting alternative choices
  // Used primarily in questions between two options
  //
  // GiNZA parses それとも as:
  // 1. Single token: それとも (CCONJ/ADV/SCONJ)
  // 2. Multi-token: それ (CCONJ) + と (ADP,dep=fixed) + も (ADP,dep=fixed)
  //
  // The rule matches both patterns to handle GiNZA parsing variations.

  r.either(
    // Pattern 1: Single token それとも
    (b1) => {
      const soretomo = b1.tok({
        lemma: 'それとも',
      }, 'soretomo');
      b1.capture(soretomo);
    },

    // Pattern 2: Multi-token - それ + と + も (three separate tokens)
    (b2) => {
      const sore = b2.tok({
        lemma: 'それ',
      }, 'sore');
      const to = b2.tok({
        text: 'と',
      }, 'to');
      const mo = b2.tok({
        text: 'も',
      }, 'mo');
      b2.inOrder(sore, to, 2);
      b2.inOrder(to, mo, 1);
      b2.captureSpan('それとも', sore, mo);
    }
  );
});
