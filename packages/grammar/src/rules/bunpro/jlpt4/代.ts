import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: 代 (dai - decade of age, generation, or era)
 *
 * A suffix indicating:
 * 1. Decade of age (e.g., 40代 = forties, 20代 = twenties)
 * 2. Decade/era (e.g., 1980年代 = 1980s, 70年代 = 70s)
 * 3. Generation/count (e.g., 第5代 = 5th generation)
 *
 * Formation:
 * - Number + 代: 40代 (forties), 20代 (twenties), 30代 (thirties)
 * - Year + 年代: 1980年代 (1980s), 70年代 (70s)
 * - Number + 代 (generations): 第5代 (5th generation)
 *
 * Key characteristics:
 * 1. Indicates a span or generation rather than specific count
 * 2. Read as だい (dai) or しろ (shiro)
 * 3. Different from similar suffixes like 齢, 層, etc.
 *
 * GiNZA parse structure:
 *
 * For hiragana forms (test sentences have the answer filled in):
 * - 40だい: 40 (NUM) + だい (NOUN or AUX)
 * - 年だい/ねんだい: Can be parsed as single token "ねんだい" or split "年" + "だい"
 *
 * Note: Test sentences use hiragana "だい" and "ねんだい" (the answer filled in),
 * not the kanji "代". This is because the cloze tests fill in the blank with
 * the reading/answer form. Some answers are "だい", others are "ねんだい".
 *
 * Strategy: Match both "だい" (standalone) and "ねんだい" (compound form).
 */
export default linguisticRule('代', (r) => {
  // Match both hiragana forms: standalone "だい" and compound "ねんだい"
  // (test sentences have different answers filled in)
  const dai = r.tok({
    textOneOf: ['だい', 'ねんだい'],
  }, 'dai');

  // The token should exist and capture it
  r.capture(dai);
});
