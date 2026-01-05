import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: それなのに (sore nanoni) - "and yet, despite that, even though"
 *
 * A conjunction that connects two contrasting clauses, emphasizing that despite
 * the previous statement (A), an unexpected or contrary result (B) occurs.
 * It's formed from それ (that) + な + のに (despite/although).
 *
 * Structures:
 * - [Statement A]. それなのに、[Statement B].
 * - [Statement A]、それなのに [Statement B].
 * - それなのに、[Statement B]. (at beginning of sentence)
 *
 * Examples:
 * - 早起きするために早く寝た。それなのに寝坊した。
 *   (I went to bed early to wake up early. And yet, I overslept.)
 * - 彼は一流のアスリートだ。それなのにかなり腰が低い。
 *   (He is a first-class athlete. And yet, he is rather humble.)
 * - 私は毎日３時間日本語の勉強をしている。それなのに全然上達している感じがしない。
 *   (I study Japanese for three hours every day. And yet, I don't feel like I'm improving at all.)
 *
 * Key discriminators:
 * - それ (demonstrative pronoun/conjunction referring to previous context)
 * - な (auxiliary/copula connecting to のに)
 * - のに (contrastive conjunction particle meaning "despite/although")
 * - The entire phrase acts as a conjunction at sentence/clause boundaries
 *
 * GiNZA parse structure:
 * - それ: PRON or CCONJ (pronoun or conjunction), dep=cc or dep=dep
 * - な: AUX with lemma=だ, dep=fixed (part of fixed expression)
 * - の: SCONJ, dep=mark
 * - に: ADP, dep=case
 *
 * Different from similar conjunctions:
 * - それでも (soredemo) - "even so, nevertheless" (neutral, less emotional)
 * - しかし (shikashi) - "however" (more formal, no surprise/emotion)
 * - だけど (dakedo) - "but" (casual, less emphatic)
 * - くせに (kuse ni) - "despite" (critical, accusatory tone)
 * - のに (noni) - "despite" (used after verb/adj, not standalone)
 *
 * The rule must distinguish それなのに (conjunction) from:
 * - それ + な + のに (separate components in different contexts)
 * - Noun + な + のに (e.g., 彼は学生なのに - "although he is a student")
 */
export default linguisticRule('それなのに', (r) => {
  // それなのに is a fixed conjunction expression
  // It consists of: それ (that) + な (aux/copula) + のに (despite)

  r.either(
    // Pattern 1: それ (CCONJ) + な + のに
    // When GiNZA tags それ as a conjunction
    (b1) => {
      const sore = b1.tok({
        text: 'それ',
        pos: 'CCONJ',
        depOneOf: ['cc', 'dep'],
      }, 'sore');

      const na = b1.tok({
        text: 'な',
        dep: 'fixed',
      }, 'na');
      b1.inOrder(sore, na, 1);

      const no = b1.tok({
        text: 'の',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'no');
      b1.inOrder(na, no, 1);

      const ni = b1.tok({
        text: 'に',
        pos: 'ADP',
        dep: 'case',
      }, 'ni');
      b1.inOrder(no, ni, 1);

      b1.captureSpan('それなのに', sore, ni);
    },

    // Pattern 2: それ (PRON) + な + のに
    // When GiNZA tags それ as a pronoun
    (b2) => {
      const sore = b2.tok({
        text: 'それ',
        pos: 'PRON',
        depOneOf: ['cc', 'dep'],
      }, 'sore');

      const na = b2.tok({
        text: 'な',
        dep: 'fixed',
      }, 'na');
      b2.inOrder(sore, na, 1);

      const no = b2.tok({
        text: 'の',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'no');
      b2.inOrder(na, no, 1);

      const ni = b2.tok({
        text: 'に',
        pos: 'ADP',
        dep: 'case',
      }, 'ni');
      b2.inOrder(no, ni, 1);

      b2.captureSpan('それなのに', sore, ni);
    }
  );
});
