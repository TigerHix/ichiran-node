import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: に-frequency (Time period + に + frequency = "N times per period")
 *
 * Matches the pattern where に indicates frequency: "Timeframe + に + Number of times".
 *
 * Structure:
 * - Time period (NOUN) + に (particle) + Number + Counter (optional)
 *
 * This pattern expresses how frequently something happens within a given time period.
 * The particle に marks the time period as the "container" or "scope" within which
 * the action occurs a certain number of times.
 *
 * Examples:
 * - 一週間に二回 (twice a week / twice every week)
 * - 一日に三回 (three times a day / three times every day)
 * - 一ヶ月に一回 (once a month)
 * - 一時間に十本 (ten trains every hour)
 * - 週に二回 (twice a week)
 *
 * Key discriminators:
 * - に must follow a time period noun (day, week, month, hour, year)
 * - A number must follow に (within 1-3 tokens)
 * - The number indicates frequency (how many times)
 *
 * GiNZA parse structure:
 * - POSITIVE: 一週間に二回
 *   - 一週間(NOUN, dep=obl) + に(ADP, dep=case) + 二(NUM) + 回(NOUN)
 * - POSITIVE: 一日に三回
 *   - 一日(NOUN, dep=obl) + に(ADP, dep=case) + 三(NUM) + 回(NOUN)
 * - POSITIVE: 週に二回
 *   - 週(NOUN, dep=obl) + に(ADP, dep=case) + 二(NUM) + 回(NOUN)
 *
 * Important: Do NOT match other uses of に:
 * - Direction/destination: 東京に行く (go to Tokyo)
 * - Time point: 三時に会う (meet at 3 o'clock)
 * - Indirect object: 彼に本をあげる (give a book to him)
 *
 * The key discriminator is that に follows a time period noun AND is followed by a number.
 */
export default linguisticRule('に-frequency', (r) => {
  // The key pattern is: time period + に + frequency expression
  // The frequency can be NUM + counter, or a compound NOUN like 一回

  // Match any noun/time expression followed by に
  const timePeriod = r.noun({}, 'timePeriod');

  const ni = r.particle('に', 'ni');
  r.caseMarker(timePeriod, ni);
  r.inOrder(timePeriod, ni, 2);  // Allow 1 token between (e.g., 一ヶ月)

  // Frequency expression after に - can be NUM or NOUN (for compounds like 一回)
  r.either(
    // Branch 1: Regular NUM token
    (b) => {
      const number = b.tok({ pos: 'NUM' }, 'number');
      b.inOrder(ni, number, 3);
      b.captureSpan('に-frequency', timePeriod, number);
    },
    // Branch 2: Compound NOUN tokens that express frequency
    (b) => {
      const freqNoun = b.noun({
        textOneOf: [
          '一回',     // once
          '二回',     // twice
          '三回',     // three times
          '一回り',   // once around
          // Add more as needed
        ],
      }, 'freqNoun');
      b.inOrder(ni, freqNoun, 3);
      b.captureSpan('に-frequency', timePeriod, freqNoun);
    }
  );
});
