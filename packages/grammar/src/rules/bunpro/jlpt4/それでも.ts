import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: それでも - Even so / Nevertheless
 *
 * Matches the conjunction それでも (soredemo) which means "even so," "nevertheless," or "despite that."
 *
 * Structure:
 * - それ + で + も (fixed expression)
 *
 * This is a conjunction that appears at the beginning of a sentence or clause,
 * connecting two contrasting ideas. It emphasizes that despite the previous
 * statement, something still occurs.
 *
 * Examples:
 * - このコーヒーは甘いですが、それでも砂糖を加えます。
 *   (This coffee is sweet, but nevertheless, I will add sugar.)
 * - タバコは体に悪いと言われている。それでも、止めにくい。
 *   (Smoking is said to be bad for you. Even so, it's hard to quit.)
 * - 高いのに、それでも買うつもりですか。
 *   (It's expensive, but are you still planning to buy it?)
 *
 * Key discriminators:
 * - それ must have dep=cc or dep=dep (unspecified dependency, used for discourse markers)
 * - で must have dep=fixed (part of fixed expression)
 * - も must have dep=fixed or dep=case
 *
 * GiNZA parse structure (inconsistent across sentences):
 * - それでも行きます: それ(CCONJ,dep=cc) + で(ADP,dep=fixed) + も(ADP,dep=fixed)
 * - タバコは...それでも、止めにくい: それ(PRON,dep=cc) + で(ADP,dep=fixed) + も(ADP,dep=fixed)
 * - 寒すぎて...それでも、暖かくならなかった: それ(PRON,dep=dep) + で(ADP,dep=fixed) + も(ADP,dep=fixed)
 */
export default linguisticRule('それでも', (r) => {
  // Match それ (pronoun/demonstrative used as conjunction)
  // GiNZA inconsistently tags it as dep=cc or dep=dep depending on context
  const sore = r.tok({
    text: 'それ',
    depOneOf: ['cc', 'dep'],
  }, 'sore');

  // Followed by で (particle, part of fixed expression)
  const de = r.tok({
    text: 'で',
    dep: 'fixed',
  }, 'de');
  r.inOrder(sore, de, 1);

  // Followed by も (particle, part of fixed expression)
  const mo = r.tok({
    text: 'も',
    depOneOf: ['fixed', 'case'],
  }, 'mo');
  r.inOrder(de, mo, 1);

  // Capture the full expression
  r.captureSpan('それでも', sore, mo);
});
