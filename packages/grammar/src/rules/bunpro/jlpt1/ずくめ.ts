import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: ずくめ (entirely X; nothing but X)
 *
 * Noun + ずくめ = entirely X, nothing but X, completely X
 *
 * Examples:
 * - 黒ずくめ (dressed entirely in black)
 * - 良いことずくめ (nothing but good things)
 * - 不満ずくめ (full of complaints)
 * - 野菜ずくめの料理 (dish entirely of vegetables)
 * - ごちそうずくめで (with nothing but delicacies)
 *
 * Key pattern:
 * Noun + ずくめ (suffix)
 *
 * GiNZA parse structure:
 * - 黒ずくめ: 黒 (NOUN) + ずくめ (NOUN, tag=接尾辞-名詞的-一般)
 * - 野菜ずくめの: 野菜 (NOUN) + ずくめ (NOUN, tag=接尾辞-名詞的-一般) + の (ADP)
 * - 良いことずくめだから: こと (NOUN) + ずくめ (NOUN, tag=接尾辞-名詞的-一般) + だ (AUX)
 * - ごちそうずくめで: ごちそう (NOUN) + ずくめ (NOUN, tag=接尾辞-名詞的-一般) + で (ADP)
 *
 * The ずくめ suffix is parsed as:
 * - NOUN with tag=接尾辞-名詞的-一般 (noun-like suffix)
 *
 * This rule matches noun + ずくめ suffix
 */
export default bunproLinguisticRule('ずくめ', (r) => {
  // Noun (any type)
  const noun = r.noun({}, 'noun');

  // ずくめ as suffix (NOUN, tag=接尾辞-名詞的-一般)
  const zukume = r.tok({
    text: 'ずくめ',
    tag: '接尾辞-名詞的-一般',
  }, 'zukume');

  r.inOrder(noun, zukume, 1);
  r.captureSpan('ずくめ', noun, zukume);
});
