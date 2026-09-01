import { bunproLinguisticRule } from '../../../engine/lang.js';

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
export default bunproLinguisticRule('かねる', (r) => {
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

    // Branch 5: かねる as VERB token that follows a verb stem
    // This catches cases where GiNZA parses it as VERB instead of AUX
    // Must be preceded by a verb stem (連用形) to ensure it's acting as auxiliary
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kaneru = b.verb({
        lemmaOneOf: ['かねる', '兼ねる'],
      }, 'kaneru');
      b.inOrder(stem, kaneru, 3);
      b.captureSpan('かねる', stem, kaneru);
    },

    // Branch 6: かね (PART, 未然形) + ます (AUX) - polite form split by GiNZA
    // e.g., 承諾しかねます (shounin-shi-kanemasu)
    (b) => {
      const stem = b.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kane = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        pos: 'PART',
        inflectionForm: '未然形-一般',
      }, 'kane');
      const masu = b.aux({
        lemma: 'ます',
      }, 'masu');
      b.inOrder(stem, kane, 3);
      b.inOrder(kane, masu, 2);
      b.captureSpan('かねる', stem, masu);
    },

    // Branch 7: かねる (VERB, dep=root) - when GiNZA parses it as main verb
    // Must be preceded by a verb stem (連用形)
    (b) => {
      const stem = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kaneru = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        pos: 'VERB',
      }, 'kaneru');
      b.inOrder(stem, kaneru, 3);
      b.captureSpan('かねる', stem, kaneru);
    },

    // Branch 8: Noun/PROPN + かね + ます (noun + kaneru polite form)
    // e.g., おうじかねます (ouji-kanemasu - cannot answer)
    // Distance 1 ensures no particle between noun and かね
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'VERB', 'ADJ'],
      }, 'noun');
      const kane = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        pos: 'PART',
        inflectionForm: '未然形-一般',
      }, 'kane');
      const masu = b.aux({
        lemma: 'ます',
      }, 'masu');
      b.inOrder(noun, kane, 1);
      b.inOrder(kane, masu, 2);
      b.captureSpan('かねる', noun, masu);
    },

    // Branch 9: Noun/PROPN + かねる (noun + kaneru plain form)
    // e.g., おうじかねる (ouji-kaneru - cannot answer)
    // Distance 1 ensures no particle (を, が, etc.) between noun and かねる
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'VERB', 'ADJ'],
      }, 'noun');
      const kaneru = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        pos: 'VERB',
        inflectionForm: '終止形-一般',
      }, 'kaneru');
      b.inOrder(noun, kaneru, 1);
      b.captureSpan('かねる', noun, kaneru);
    },

    // Branch 10: Any token + かねる (NOUN) - GiNZA sometimes tags かねる as NOUN
    // e.g., しんじかねる (shinji-kaneru - cannot believe)
    // Distance 1 ensures no particle between noun and かねる
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'VERB', 'ADJ', 'AUX'],
      }, 'noun');
      const kaneru = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        pos: 'NOUN',
        inflectionForm: '終止形-一般',
      }, 'kaneru');
      b.inOrder(noun, kaneru, 1);
      b.captureSpan('かねる', noun, kaneru);
    }
  );
});
