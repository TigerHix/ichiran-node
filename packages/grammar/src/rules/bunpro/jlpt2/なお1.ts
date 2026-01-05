import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: なお① (nao - "still, even, yet")
 *
 * A formal adverb indicating that something is still continuing or remains true
 * even after some event or condition. Expresses persistence or continuation
 * despite expectations to the contrary.
 *
 * Structures:
 * - なお + Verb［ている］(still in a state)
 * - なお + Nounもいる (still exists/there is still)
 * - なお + Nounもある (still exists/there is still)
 * - なお + Noun + だ/である (still is)
 *
 * Examples:
 * - ある旅館は718年以来今もなお一家族によって経営されている。
 *   (A certain ryokan is still managed by one family since 718.)
 * - 退院してもなお病院に遊びに来る人が絶えないほど人気の病院があるらしい。
 *   (Apparently there are hospitals that are so popular that even after being released, people still come to visit.)
 * - 上司のパワハラを人事に報告してもなお、上司のパワハラが続いている。
 *   (Even after reporting my boss to human resources for power harassment, his harassment is still continuing.)
 *
 * Key discriminators:
 * - Written in hiragana (なお) - the kanji form (尚) is rare in this usage
 * - Formal register (硬い) - used in written or formal contexts
 * - Functions as an adverb (ADV)
 * - Often appears after conditional forms like 〜ても or time expressions like 今も
 * - Emphasizes that something continues despite expectations
 * - Often followed by も in the main clause for emphasis
 * - NOT the same as なおも (nao mo - "still more") which is a compound word
 *
 * GiNZA parse structure:
 * - なお (ADV) - adverb
 * - Modifies the following predicate or clause
 * - When followed by particle も, it forms the compound なおも (different meaning)
 *
 * Different from:
 * - なお② (nao - "furthermore, in addition") - same word, different context (adding new info)
 * - なおも (nao mo) - "still more" (compound adverb: なお + も)
 * - まだ (mada) - "still" (less formal, everyday usage)
 * - いまでも (ima demo) - "even now" (less formal)
 * - いまだに (imada ni) - "still yet" (emphasizes unexpectedness)
 *
 * Note: なお① and なお② are contextual uses of the same word なお.
 * The rule matches the adverb なお in all its contexts, as the grammar
 * point teaches the word itself rather than a specific construction pattern.
 */
export default linguisticRule('なお1', (r) => {
  // なお (still, even, yet / furthermore, in addition)
  // Formal adverb with two main contextual meanings:
  // 1. なお①: something continues despite expectations (still/even)
  // 2. なお②: adds supplemental information (furthermore)
  // Both use the same word なお - context determines the meaning
  // Usually written in hiragana, rarely as kanji (尚)
  // GiNZA tags this as ADV (adverb)

  const nao = r.adv({
    textOneOf: ['なお', '尚'],
    lemma: 'なお',
  }, 'nao');

  // Capture just the adverb itself
  // It modifies whatever follows in the sentence
  // The meaning (still vs furthermore) depends on context
  r.capture(nao);
});
