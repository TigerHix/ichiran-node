import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: だらけ (full of/covered with - usually negative)
 *
 * Noun + だらけ = full of X, covered in X, nothing but X
 *
 * Examples:
 * - ゴミだらけ (full of garbage/trash)
 * - 傷だらけ (covered in wounds)
 * - 穴だらけ (full of holes)
 * - 泥だらけ (covered in mud)
 * - 欠点だらけ (full of faults)
 * - 矛盾だらけ (full of contradictions)
 * - 汗だらけ (covered in sweat)
 *
 * Usage notes:
 * - Usually has negative connotation (dirt, wounds, problems, etc.)
 * - Indicates being covered with or full of something undesirable
 * - Can be followed by の (e.g., 傷だらけの車)
 * - Can be followed by で (e.g., ゴミだらけだから)
 * - Can be at sentence end (e.g., 矛盾だらけだ)
 *
 * GiNZA parse structure:
 * - ゴミだらけ: ゴミ (NOUN) + だらけ (PART, tag=接尾辞-形状詞的)
 * - 傷だらけ: 傷 (NOUN) + だらけ (PART, tag=接尾辞-形状詞的)
 * - 穴だらけ: 穴 (NOUN) + だらけ (PART, tag=接尾辞-形状詞的)
 * - 泥だらけ: 泥 (NOUN) + だらけ (PART, tag=接尾辞-形状詞的)
 *
 * The だらけ suffix is parsed as:
 * - PART (particle) with tag=接尾辞-形状詞的 (na-adjective-like suffix)
 * - lemma: "だらけ"
 *
 * This rule matches nouns + だらけ suffix
 */
export default bunproLinguisticRule('だらけ', (r) => {
  // だらけ as suffix (PART, tag=接尾辞-形状詞的)
  const darake = r.tok({
    lemma: 'だらけ',
    tag: '接尾辞-形状詞的',
  }, 'darake');

  r.capture(darake);
});
