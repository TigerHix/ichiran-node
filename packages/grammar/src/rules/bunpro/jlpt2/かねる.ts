import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: かねる (kaneru) - Cannot, Be unable to, Hard to, Hesitant to
 *
 * A formal/polite auxiliary verb expressing inability or hesitation to do something.
 * Indicates that the speaker finds it difficult to do (A) due to external circumstances,
 * conflict of opinions, or spreading efforts too thin. Often used for polite refusals
 * in business contexts.
 *
 * Structures:
 * - Verb［stem］+ かねる (casual)
 * - Verb［stem］+ かねます (polite)
 *
 * Examples:
 * - その提案には賛成しかねます。
 *   (I cannot agree to that proposal.)
 * - 判断しかねます。
 *   (I am unable to make a judgment.)
 * - お答えしかねます。
 *   (I am unable to answer.)
 * - 専門的なプログラムには対応し兼ねる。
 *   (I cannot deal with a technical program.)
 *
 * Key discriminators:
 * - かねる is an ichidan (ru-verb) auxiliary verb
 * - Attaches to verb stem (masu stem/ren'youkei)
 * - Indicates hesitation or inability (polite refusal)
 * - Should be used only with transitive words (except わかる and できる)
 * - Formal register, used in business contexts
 * - Different from かねない (possibility of negative outcome)
 * - Different from independent use of 兼ねる (to combine/serve dual purpose)
 *
 * GiNZA parse structure:
 * - し(VERB) + かねる(AUX) or かねる(VERB)
 * - 対応し(VERB) + 兼ねる(AUX/VERB)
 * - Various dependency relations (aux, fixed, compound, advcl)
 *
 * Important: Matches only when attached as auxiliary to verb stem
 * to exclude:
 * - かねない (negative form - different grammar, pattern is verb+かね+ない)
 * - かねて (te-form - as in を兼ねて, stem is かね)
 * - Independent use of 兼ねる (to combine/serve dual purpose)
 */
export default linguisticRule('かねる', (r) => {
  r.either(
    // Branch 1: かねる as auxiliary with dep=aux attached to verb
    // Most common pattern for verb stem + aux
    (b) => {
      const kaneru = b.aux({
        lemmaOneOf: ['かねる', '兼ねる'],
        dep: 'aux',
      }, 'kaneru');
      b.capture(kaneru);
    },

    // Branch 2: かねる as auxiliary with dep=fixed
    // Alternative parsing for some verb forms
    (b) => {
      const kaneru = b.aux({
        lemmaOneOf: ['かねる', '兼ねる'],
        dep: 'fixed',
      }, 'kaneru');
      b.capture(kaneru);
    },

    // Branch 3: Verb stem (ren'youkei) + かねる with advcl dependency
    // Stem is syntactic head, かねる modifies it
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kaneru = b.aux({
        lemmaOneOf: ['かねる', '兼ねる'],
      }, 'kaneru');
      b.headChild(stem, kaneru, 'advcl');
      b.captureSpan('かねる', stem, kaneru);
    },

    // Branch 4: Verb stem + かねる with compound dependency
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kaneru = b.aux({
        lemmaOneOf: ['かねる', '兼ねる'],
      }, 'kaneru');
      b.headChild(stem, kaneru, 'compound');
      b.captureSpan('かねる', stem, kaneru);
    },

    // Branch 5: かねる as VERB token when attached as auxiliary
    // Some parsings show it as VERB rather than AUX
    (b) => {
      const kaneru = b.verb({
        lemmaOneOf: ['かねる', '兼ねる'],
      }, 'kaneru');
      b.capture(kaneru);
    }
  );
});
