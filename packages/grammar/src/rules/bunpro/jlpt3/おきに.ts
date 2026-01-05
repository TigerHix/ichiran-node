import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: おきに (every interval, at intervals of)
 *
 * Pattern: Number/Counter + おきに = "at intervals of X", "every X"
 * Examples:
 * - 一日おきに (every other day, at intervals of one day)
 * - ３０分おきに (every 30 minutes)
 * - 一ヶ月おきに (every other month)
 * - ２年おきに (every two years)
 *
 * Key patterns:
 * 1. Number + counter + おきに - time/distance intervals
 * 2. おき is a suffix (接尾辞) meaning "interval" or "opening"
 * 3. に is a case particle (格助詞)
 *
 * GiNZA parse structure:
 * - 一日おきに: 一(NUM) + 日(NOUN) + おき(NOUN, suffix) + に(ADP/PART)
 * - ３０分おきに: ３０(NUM) + 分(NOUN) + おき(NOUN, suffix) + に(ADP/PART)
 * - 一ヶ月おきに: 一(NUM) + ヶ月(NOUN) + おき(NOUN, suffix) + に(ADP/PART)
 *
 * Key discriminators vs ごとに:
 * - おきに: emphasizes the interval/space between occurrences
 * - ごとに: means "each" or "every" without emphasis on interval
 * - Both can attach to numbers + counters
 */
export default bunproLinguisticRule('おきに', (r) => {
  // おき suffix - typically parsed as NOUN with suffix tag
  const oki = r.tok({
    lemma: 'おき',
    tag: '接尾辞-名詞的-一般',
  }, 'oki');

  // Case particle に
  const ni = r.particle('に', 'ni');

  r.inOrder(oki, ni, 1);

  // Capture the full pattern: number/counter + おきに
  r.captureSpan('おきに', oki, ni);
});
