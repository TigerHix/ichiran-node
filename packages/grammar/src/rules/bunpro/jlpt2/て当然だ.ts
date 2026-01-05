import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: て当然だ (te touzen da) - "it is natural that..., it goes without saying that..."
 *
 * An expression stating that something is completely correct or happens exactly as expected.
 * Combines the te-form of verbs/adjectives (or で for na-adjectives) with "当然" (naturally/matter of course)
 * and the copula "だ/です".
 *
 * Structures:
 * - Verb［て］+ 当然 + だ/です
 * - I-adjective［て］+ 当然 + だ/です
 * - Na-adjective + で + 当然 + だ/です
 * - Noun + で + 当然 + だ/です
 *
 * Examples:
 * - そんなに食べたら、太って当然だ。
 *   (If you eat that much, of course you'll get fat.)
 * - 育児をしているのだから、忙しくて当然だ。
 *   (Because you're raising a child, it's natural that you're busy.)
 * - このビルは先月建てられたばかりだから、綺麗で当然だ。
 *   (This building was just built last month, so of course it's beautiful.)
 * - 彼女は優しいから、好かれて当然だ。
 *   (Because she's kind, it's natural that she's liked.)
 *
 * Key discriminators:
 * - Verb/adj-te form + 当然 (touzen) - the noun "naturally/matter of course"
 * - Copula だ/です (da/desu) following 当然
 * - Na-adjectives use で (te-form of copula) instead of adjective-te form
 * - Expresses strong expectation or obviousness
 * - Different from のも当然だ (nominalized form with "no")
 *
 * GiNZA parse structure:
 * - "太って当然だ" → 太って(VERB) + 当然(NOUN/ADV) + だ(AUX)
 * - "忙しくて当然だ" → 忙しくて(ADJ) + 当然(NOUN/ADV) + だ(AUX)
 * - "綺麗で当然だ" → 綺麗(NOUN/ADJ) + で(ADP) + 当然(NOUN/ADV) + だ(AUX)
 * - 当然 can be parsed as NOUN or ADV depending on context
 */
export default bunproLinguisticRule('て当然だ', (r) => {
  r.either(
    // Pattern 1: て particle + とうぜん/当然 + だ/です (most common pattern)
    // Matches: Verb-te + とうぜん + だ, I-adj-te + とうぜん + だ
    // Example parse: ふとっ(VERB) + て(SCONJ) + とうぜん(VERB/NOUN) + だ(AUX)
    (b1) => {
      const te = b1.tok({
        text: 'て',
        posOneOf: ['SCONJ', 'AUX']
      }, 'te');
      const touzen = b1.tok({
        textOneOf: ['当然', 'とうぜん'],
        posOneOf: ['NOUN', 'ADV', 'VERB']
      }, 'touzen');
      const copula = b1.aux({ lemmaOneOf: ['だ', 'です'] }, 'copula');

      b1.inOrder(te, touzen, 1);
      b1.inOrder(touzen, copula, 1);
      b1.captureSpan('て当然だ', te, copula);
    },

    // Pattern 2: で particle (for na-adjectives/nouns) + とうぜん/当然 + だ/です
    // Matches: Na-adj/Noun + で + とうぜん + だ
    (b2) => {
      const de = b2.particle('で', 'de');
      const touzen = b2.tok({
        textOneOf: ['当然', 'とうぜん'],
        posOneOf: ['NOUN', 'ADV', 'VERB']
      }, 'touzen');
      const copula = b2.aux({ lemmaOneOf: ['だ', 'です'] }, 'copula');

      b2.inOrder(de, touzen, 1);
      b2.inOrder(touzen, copula, 1);
      b2.captureSpan('で当然だ', de, copula);
    }
  );
});
