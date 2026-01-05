import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ざる (zaru) - Literary/archaic negative form
 *
 * A classical auxiliary verb (attributive form of ず) expressing negation.
 * Used primarily in set phrases and formal/literary contexts. Often translated
 * as "un~" when modifying nouns (e.g., 知られざる = "unknown").
 *
 * Structure:
 * - Verb［stem/連用形］+ ざる
 *
 * Examples:
 * - 知られざる名画 (an unknown masterpiece)
 * - たゆまざる努力 (unwavering effort)
 * - 消えざる傷 (a wound that will not disappear)
 * - 絶えざる失敗 (unending failures)
 * - 取り返しのつかざるミス (irreparable mistake)
 *
 * Key discriminators:
 * - ざる is the classical/literary negative auxiliary (attributive form of ず)
 * - Attaches to verb stem (連用形/ren'youkei)
 * - Follows the same pattern as modern ない but in classical form
 * - Usually modifies nouns (attributive function)
 * - Used in set phrases like ざるを得ない
 * - Different from standalone ざる (rare)
 * - Different from modern ない/ぬ negative forms
 *
 * GiNZA parse structure:
 * - Verb stem in 連用形-一般 + ざる(AUX) or ざる(VERB)
 * - Various dependency relations (aux, fixed, compound, advcl)
 *
 * Important: Matches only when attached as auxiliary to verb stem
 * to exclude:
 * - ざるを得ない (different grammar point - full expression)
 * - Modern ない negative forms
 * - ぬ negative forms (related but different)
 * - Standalone use of ざる (very rare)
 */
export default bunproLinguisticRule('ざる', (r) => {
  r.either(
    // Branch 1: ざる as auxiliary with dep=aux attached to verb stem
    // Most common pattern: verb stem + aux
    (b) => {
      const zaru = b.aux({
        text: 'ざる',
        dep: 'aux',
      }, 'zaru');
      b.capture(zaru);
    },

    // Branch 2: ざる as auxiliary with dep=fixed
    // Alternative parsing for some verb forms
    (b) => {
      const zaru = b.aux({
        text: 'ざる',
        dep: 'fixed',
      }, 'zaru');
      b.capture(zaru);
    },

    // Branch 3: Verb stem (連用形) + ざる with advcl dependency
    // Stem is syntactic head, ざる modifies it
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const zaru = b.aux({
        text: 'ざる',
      }, 'zaru');
      b.headChild(stem, zaru, 'advcl');
      b.captureSpan('ざる', stem, zaru);
    },

    // Branch 4: Verb stem + ざる with compound dependency
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const zaru = b.aux({
        text: 'ざる',
      }, 'zaru');
      b.headChild(stem, zaru, 'compound');
      b.captureSpan('ざる', stem, zaru);
    },

    // Branch 5: ざる as VERB token that follows a verb stem
    // This catches cases where GiNZA parses it as VERB instead of AUX
    // Must be preceded by a verb stem (連用形) to ensure it's acting as auxiliary
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const zaru = b.verb({
        text: 'ざる',
      }, 'zaru');
      b.inOrder(stem, zaru, 3);
      b.captureSpan('ざる', stem, zaru);
    }
  );
});
