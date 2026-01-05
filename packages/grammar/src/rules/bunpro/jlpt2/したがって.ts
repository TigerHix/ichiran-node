import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: したがって (shitagatte) - "therefore, thus, as a result"
 *
 * A formal conjunction used to indicate a logical consequence or conclusion
 * from what was previously stated. It is the te-form of the verb "したがう"
 * (to follow/comply with), literally meaning "following from (A), (B)".
 *
 * Structures:
 * - [Cause/Reason]。したがって、[Result].
 * - [Cause/Reason]。したがって[Result].
 *
 * Examples:
 * - 彼は長男です。したがって、次期社長はおそらく彼でしょう。
 *   (He is the oldest son. Therefore, he will likely be the next CEO.)
 * - 本番直前です。したがって、彼は極度に緊張しています。
 *   (It's right before the performance. Therefore, he is extremely nervous.)
 *
 * Key characteristics:
 * - Formal conjunction meaning "therefore, consequently, thus"
 * - Indicates logical consequence from previous statement
 * - Similar to だから, それで, but more formal/written style
 * - Often appears at the beginning of sentences after a period
 *
 * Kanji forms:
 * - したがって (hiragana - standard)
 * - 従って (kanji - common in formal writing)
 */
export default linguisticRule('したがって', (r) => {
  const shitagatte = r.tok({
    textOneOf: ['したがって', '従って'],
  }, 'shitagatte');

  r.capture(shitagatte);
});
