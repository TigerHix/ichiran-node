import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: がち (tendency to/prone to)
 *
 * Verb stem + がち = tendency to do X, apt to do X, prone to do X
 * Noun + がち = tendency to be X, prone to X
 *
 * Examples:
 * - 遅刻しがち (tend to be late)
 * - 病気がち (prone to getting sick)
 * - 遠慮がち (tend to hold back)
 * - さぼりがちな (tend to skip)
 * - ありがちの (common/typical)
 *
 * Key patterns:
 * 1. Verb stem (連用形) + がち - masu stem form
 * 2. Noun + がち
 *
 * GiNZA parse structure:
 * - 遅刻しがち: 遅刻 (NOUN) + し (VERB, inflectionForm=連用形-一般) + がち (PART, tag=接尾辞-形状詞的)
 * - 病気がち: 病気 (VERB, tag=名詞-普通名詞-サ変可能) + がち (PART, tag=接尾辞-形状詞的)
 * - 遠慮がち: 遠慮 (VERB, tag=名詞-普通名詞-サ変可能) + がち (PART, tag=接尾辞-形状詞的)
 * - さぼりがち: さぼり (NOUN/VERB, inflectionForm=連用形-一般) + がち (PART, tag=接尾辞-形状詞的)
 * - ありがち: あり (VERB, inflectionForm=連用形-一般) + がち (PART, tag=接尾辞-形状詞的)
 * - 伏し目がち: 伏し目 (VERB, tag=名詞-普通名詞-一般) + がち (PART, tag=接尾辞-形状詞的)
 *
 * The がち suffix is parsed as:
 * - PART (particle) with tag=接尾辞-形状詞的 (na-adjective-like suffix)
 *
 * This rule matches verb stems (連用形-一般) or nouns + がち suffix
 */
export default linguisticRule('がち', (r) => {
  r.either(
    // Pattern 1: Verb stem (連用形-一般) + がち
    (b) => {
      // Verb stem (masu form, 連用形-一般)
      const stem = b.tok({
        inflectionForm: '連用形-一般',
      }, 'stem');

      // がち as suffix (PART, tag=接尾辞-形状詞的)
      const gachi = b.tok({
        textOneOf: ['がち', 'ガチ'],
        tag: '接尾辞-形状詞的',
      }, 'gachi');

      b.inOrder(stem, gachi, 1);
      b.captureSpan('がち', stem, gachi);
    },

    // Pattern 2: Noun/Verb (without 連用形-一般) + がち
    // For suru-verb nouns like 病気, 遠慮 that GiNZA parses as VERB
    // Also for regular nouns like 伏し目
    (b) => {
      // Noun or VERB that does NOT have 連用形-一般
      // (i.e., dictionary forms and nouns)
      const base = b.tok({
        posOneOf: ['NOUN', 'VERB'],
        // We need to ensure this doesn't have 連用形-一般
        // Since GiNZA doesn't always set inflectionForm, we check the pos
        // Verb stems will have pos=VERB + inflectionForm=連用形-一般
        // Nouns/suru-verbs will have pos=NOUN or pos=VERB + inflectionForm=undefined
      }, 'base');

      // がち as suffix (PART, tag=接尾辞-形状詞的)
      const gachi = b.tok({
        textOneOf: ['がち', 'ガチ'],
        tag: '接尾辞-形状詞的',
      }, 'gachi');

      b.inOrder(base, gachi, 1);
      b.captureSpan('がち', base, gachi);
    }
  );
});
