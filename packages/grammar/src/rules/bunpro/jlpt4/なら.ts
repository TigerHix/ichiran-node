import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: なら (conditional: "if it's X")
 *
 * Matches noun/na-adj/verb/i-adj + なら (conditional form)
 *
 * なら is the conditional form of the copula だ (to be).
 * It expresses "if it's the case that X" or "speaking of X".
 *
 * GiNZA tokenizes なら as:
 * - lemma: だ (copula)
 * - pos: AUX
 * - inflectionForm: 仮定形-一般 (conditional form)
 * - conjugationClass: 助動詞-ダ
 *
 * Unlike だ (copula), なら can attach directly to verbs and i-adjectives
 * without nominalization.
 *
 * Examples:
 * - 無理なら (if it's impossible)
 * - 暑いなら (if it's hot)
 * - 行くなら (if (you) go)
 * - デザートなら (if it's dessert)
 */
export default linguisticRule('なら', (r) => {
  // Match なら as conditional form of copula だ
  const nara = r.aux({
    text: 'なら',
    lemma: 'だ',
    inflectionForm: '仮定形-一般',
    conjugationClass: '助動詞-ダ',
  }, 'nara');

  // なら attaches to the preceding word
  // It can follow: nouns, verbs, i-adjectives, na-adjectives
  r.capture(nara);
});
