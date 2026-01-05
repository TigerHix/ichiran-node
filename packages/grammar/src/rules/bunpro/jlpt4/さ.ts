import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: さ (adjective nominalizer)
 *
 * Converts adjectives to nouns expressing degree or amount.
 * English equivalent: "-ness" suffix (sweetness, height, beauty, etc.)
 *
 * Formation:
 * - I-adjective stem (remove い) + さ: 高い → 高さ (height), 美しい → 美しさ (beauty)
 * - Na-adjective + さ: 元気 → 元気さ (energy/vitality), 綺麗 → 綺麗さ (prettiness)
 *
 * Key characteristics:
 * 1. Transforms adjectives into abstract nouns indicating measurable degree
 * 2. Different from み (focuses on quality rather than degree)
 * 3. Different from こと (general nominalization vs degree-specific)
 *
 * GiNZA parse structure (VERY INCONSISTENT):
 *
 * Split parsing (adjective stem + さ as separate tokens):
 * - 大切さ: 大切 (ADJ, tag=形状詞-一般) + さ (PART, tag=接尾辞-名詞的-一般)
 * - 甘さ: 甘 (ADJ, tag=形容詞-一般, inflectionForm=語幹-一般) + さ (PART, tag=接尾辞-名詞的-一般)
 * - 美しさ: 美し (ADJ, tag=形容詞-一般, inflectionForm=語幹-一般) + さ (PART, tag=接尾辞-名詞的-一般)
 * - 元気さ: 元気 (NOUN, tag=名詞-普通名詞-形状詞可能) + さ (PART, tag=接尾辞-名詞的-一般)
 * - 綺麗さ: 綺麗 (ADJ, tag=形状詞-一般) + さ (PART, tag=接尾辞-名詞的-一般)
 *
 * Single token parsing (whole word as one token):
 * - 深さ (ADJ, tag=名詞-普通名詞-一般)
 * - 重さ (VERB, tag=名詞-普通名詞-一般)
 *
 * Strategy: Match the split cases (さ as PART with tag=接尾辞-名詞的-一般).
 * Single-token cases like 深さ/重さ are already nouns and don't need grammar matching.
 *
 * Test examples:
 * - 大人になってから家族の大切さが分かってきた (family's importance)
 * - この甘さは美味しい (this sweetness)
 * - 琵琶湖の深さって知っている？ (depth of Lake Biwa)
 * - 箱の重さを測ってください (weight of the box)
 */
export default bunproLinguisticRule('さ', (r) => {
  // GiNZA parses さ as PART (not NOUN) with tag=接尾辞-名詞的-一般
  const sa = r.tok({
    text: 'さ',
    tag: '接尾辞-名詞的-一般',
  }, 'sa');

  // さ should follow something (the adjective stem)
  const prev = r.tok({}, 'adj');
  r.inOrder(prev, sa, 1);

  // Capture from adjective to さ suffix
  r.captureSpan('さ', prev, sa);
});
