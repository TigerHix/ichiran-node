import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: たて (tate) - "freshly done, just completed"
 *
 * Verb stem + たて = "freshly X-ed", "just finished X"
 *
 * Examples:
 * - 焼きたてのパン (freshly baked bread)
 * - 揚げたてです (is freshly fried)
 * - 釣りたてだから新鮮だ (it's freshly caught, so it's fresh)
 * - 生まれたての赤ちゃん (newborn baby)
 * - この魚は焼きたてです (this fish is freshly cooked)
 *
 * GiNZA parses this in multiple ways:
 * - As NOUN with text=たて (separate token)
 * - As PART (particle/suffix) with various tags
 * - As compound tokens (e.g., 焼きたて, 出来たて, 釣りたて) - these CANNOT be matched
 *   because GiNZA tokenizes them as single tokens where text ends with "て" not "たて"
 *
 * Key point: たて is a suffix that transforms the verb stem into a noun,
 * indicating the action was just completed and the result is still "fresh".
 */
export default linguisticRule('たて', (r) => {
  r.either(
    // Branch 1: たて as NOUN (separate token)
    // Example: むしたてのお芋 (freshly steamed potatoes)
    (b) => {
      const tate = b.noun({
        text: 'たて',
      }, 'tate');
      b.capture(tate);
    },

    // Branch 2: たて as particle/suffix with any tag
    // Example: 出来たての (when たて is parsed as particle/suffix)
    (b) => {
      const tate = b.tok({
        text: 'たて',
        pos: 'PART',
      }, 'tate');
      b.capture(tate);
    },

    // Branch 3: Any token with text=たて (catch-all for various POS)
    // Covers VERB, AUX, and other parts of speech
    (b) => {
      const tate = b.tok({
        text: 'たて',
      }, 'tate');
      b.capture(tate);
    }
  );
});
