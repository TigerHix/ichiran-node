import { bunproLinguisticRule } from '../../../engine/lang.js';

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
export default bunproLinguisticRule('それとも', (r) => {
  // それとも is a conjunction presenting alternative choices
  // Used primarily in questions between two options
  //
  // GiNZA may parse それとも in different ways:
  // 1. Single token: それとも (CCONJ/ADV/SCONJ)
  // 2. Two tokens: それ (PRON/CCONJ) + とも (PART/SCONJ/ADP)
  // 3. Three tokens: それ (CCONJ) + と (ADP) + も (ADP) - most common in GiNZA!
  //
  // The rule matches all patterns to handle GiNZA parsing variations.

  r.either(
    // Pattern 1: Single token それとも (most likely)
    (b1) => {
      const soretomo = b1.tok({
        lemma: 'それとも',
      }, 'soretomo');
      b1.capture(soretomo);
    },

    // Pattern 2: Multi-token - それ (pronoun/conjunction) + とも (particle/adverb)
    (b2) => {
      const sore = b2.tok({
        lemma: 'それ',
      }, 'sore');
      const tomo = b2.tok({
        lemma: 'とも',
      }, 'tomo');
      b2.inOrder(sore, tomo, 5);
      b2.captureSpan('それとも', sore, tomo);
    },

    // Pattern 3: Three-token - それ + と + も (GiNZA's most common split)
    // Example: "クレジットカードで払う？それとも現金で？"
    // Tokenizes as: それ(CCONJ) + と(ADP,fixed) + も(ADP,fixed)
    (b3) => {
      const sore = b3.tok({
        lemma: 'それ',
      }, 'sore');
      const to = b3.tok({
        text: 'と',
        pos: 'ADP',
      }, 'to');
      const mo = b3.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');
      b3.inOrder(sore, to, 1);
      b3.inOrder(to, mo, 1);
      b3.captureSpan('それとも', sore, mo);
    },

    // Pattern 4: Three-token with any POS (more permissive)
    (b4) => {
      const sore = b4.tok({
        text: 'それ',
      }, 'sore');
      const to = b4.tok({
        text: 'と',
      }, 'to');
      const mo = b4.tok({
        text: 'も',
      }, 'mo');
      b4.inOrder(sore, to, 1);
      b4.inOrder(to, mo, 1);
      b4.captureSpan('それとも', sore, mo);
    }
  );
});
