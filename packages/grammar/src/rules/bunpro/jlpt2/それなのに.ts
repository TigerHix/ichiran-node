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
 * - The phrase "それなのに" as a fixed 4-token sequence (それ + な + の + に)
 * - Used at sentence/clause boundaries as a conjunction
 * - Expresses contrast with an emotional nuance of surprise or disappointment
 *
 * Note on constraints:
 * - This rule uses minimal constraints (text matching only) to accommodate
 *   GiNZA's inconsistent POS/dep tagging across different sentence contexts
 * - The risk of overcapture is mitigated by the specific 4-token sequence
 * - Negative tests ensure it doesn't match noun+な+のに patterns (学生なのに, etc.)
 *
 * Different from similar conjunctions:
 * - それでも (soredemo) - "even so, nevertheless" (neutral, less emotional)
 * - しかし (shikashi) - "however" (more formal, no surprise/emotion)
 * - だけど (dakedo) - "but" (casual, less emphatic)
 * - くせに (kuse ni) - "despite" (critical, accusatory tone)
 * - のに (noni) - "despite" (used after verb/adj, not standalone)
 * - Noun + な + のに (e.g., 彼は学生なのに - "although he is a student")
 */
export default linguisticRule('それなのに', (r) => {
  // それなのに is a fixed conjunction expression
  // It consists of: それ (that) + な (aux/copula) + のに (despite)

  // Match それ (demonstrative pronoun/conjunction)
  // GiNZA may tag it inconsistently, so we use minimal constraints
  const sore = r.tok({
    text: 'それ',
  }, 'sore');

  // Followed by な (aux/copula, part of fixed expression)
  const na = r.tok({
    text: 'な',
  }, 'na');
  r.inOrder(sore, na, 1);

  // Followed by の (nominalizer particle)
  const no = r.tok({
    text: 'の',
  }, 'no');
  r.inOrder(na, no, 1);

  // Followed by に (case particle)
  const ni = r.tok({
    text: 'に',
  }, 'ni');
  r.inOrder(no, ni, 1);

  // Capture the full expression
  r.captureSpan('それなのに', sore, ni);
});
