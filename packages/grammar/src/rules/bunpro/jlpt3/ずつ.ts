import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ずつ (each, per, at a time)
 *
 * Pattern: Number/Counter/Quantity + ずつ = "each X", "per X", "X at a time"
 * Examples:
 * - 一つずつ (one at a time, each one)
 * - 一人ずつ (one person at a time)
 * - 少しずつ (little by little, gradually)
 * - ２回ずつ (two times each)
 * - いくらかずつ (some amount each)
 *
 * Key patterns:
 * 1. Number + counter + ずつ - distribute in equal amounts
 * 2. Quantity adverb (少し, いくらか, わずか) + ずつ - gradual increase
 * 3. ずつ is a suffix (接尾辞) meaning "each" or "apiece"
 *
 * GiNZA parse structure:
 * - 一つずつ: 一(NUM) + つ(NOUN) + ずつ(ADP/PART, suffix)
 * - 少しずつ: 少し(ADV) + ずつ(ADP/PART, suffix)
 * - 一人ずつ: 一(NUM) + 人(NOUN) + ずつ(ADP/PART, suffix)
 *
 * Key discriminators vs similar patterns:
 * - ずつ: attaches to quantities, means "each X at a time" with allotment nuance
 * - おきに: attaches to time/distance, means "at intervals of X"
 * - ごとに: attaches to nouns, means "every X" without interval emphasis
 * - あたり: attaches to number + counter, means "per X" (apiece)
 */
export default linguisticRule('ずつ', (r) => {
  // ずつ suffix particle - typically parsed as ADP or PART
  // It's an adverbial particle (副助詞) that functions as a suffix
  const zutsu = r.tok({
    text: 'ずつ',
    posOneOf: ['ADP', 'PART'],
  }, 'zutsu');

  // Capture the full pattern: quantity/number/counter + ずつ
  // We capture from zutsu backward to include the quantity it modifies
  r.captureSpan('ずつ', zutsu, zutsu);
});
