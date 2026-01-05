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
    // Branch 1: Verb stem + かね (PART) + ます
    // Example: でき + かね + ます
    (b) => {
      const stem = b.tok({
        posOneOf: ['VERB', 'ADJ'],
      }, 'stem');
      const kane = b.tok({
        pos: 'PART',
        lemmaOneOf: ['かねる', '兼ねる'],
      }, 'kane');
      b.inOrder(stem, kane, 2);
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.inOrder(kane, masu, 1);
      b.captureSpan('かねる', stem, masu);
    },

    // Branch 2: Verb stem (連用形) + かねる (終止形) - sentence-final
    (b) => {
      const stem = b.tok({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kaneru = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        inflectionForm: '終止形-一般',
      }, 'kaneru');
      b.inOrder(stem, kaneru, 3);
      b.captureSpan('かねる', stem, kaneru);
    },

    // Branch 3: Suru-verb stem (no inflection form) + し + かねる
    // Example: 推薦(VERB) + し(AUX) + かねる(VERB)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'VERB'] }, 'noun');
      const shi = b.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi');
      b.auxOf(noun, shi);

      const kaneru = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        inflectionForm: '終止形-一般',
      }, 'kaneru');
      b.inOrder(shi, kaneru, 2);
      b.captureSpan('かねる', noun, kaneru);
    },

    // Branch 4: Suru-verb stem + し + かね (未然形) + ます
    // Example: 承諾 + し + かね + ます
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'VERB'] }, 'noun');
      const shi = b.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi');
      b.auxOf(noun, shi);

      const kane = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        inflectionForm: '未然形-一般',
      }, 'kane');
      b.inOrder(shi, kane, 2);

      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.inOrder(kane, masu, 1);

      b.captureSpan('かねる', noun, masu);
    },

    // Branch 5: Irregular zu-verbs (parsed as ADJ) + かねる
    // Example: しんじ(ADJ,lemma=しんずる) + かねる
    // GiNZA parses these irregular verbs as ADJ in their stem form
    (b) => {
      const stem = b.adj({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kaneru = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        inflectionForm: '終止形-一般',
      }, 'kaneru');
      b.inOrder(stem, kaneru, 2);
      b.captureSpan('かねる', stem, kaneru);
    },

    // Branch 6: Irregular zu-verbs (parsed as ADJ) + かね + ます
    // Example: おうじ(ADJ,lemma=おうずる) + かね + ます
    (b) => {
      const stem = b.adj({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kane = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        inflectionForm: '未然形-一般',
      }, 'kane');
      b.inOrder(stem, kane, 2);

      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.inOrder(kane, masu, 1);

      b.captureSpan('かねる', stem, masu);
    }
  );
});
