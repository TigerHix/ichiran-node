import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: なん-counter-か (何 + counter + か)
 *
 * Matches expressions of uncertain quantity: "some/several N counter"
 *
 * Structures:
 * - 何/なん + counter + か (some/several)
 * - 幾/いく + counter + か (some/several)
 * - いくつか (some things/a few things)
 *
 * Meanings: "some", "several", "a few"
 *
 * Examples:
 * - 何人か (some people / several people)
 * - 何本か (some long objects)
 * - 何枚か (some flat objects)
 * - 何回か (several times)
 * - いくつか (some things)
 *
 * Key discriminators:
 * - The question word (何/なん/幾/いく) must be followed by a counter
 * - The particle か marks uncertainty/indeterminacy
 * - Different from question-phrase-か which embeds questions (like どこかわかる)
 *
 * GiNZA parse structure (HIGHLY INCONSISTENT):
 * - なんまいか: なん(PRON) + まい(AUX) + か(ADP)
 * - なんぼんか: なん(PRON) + ぼん(AUX) + か(ADP)
 * - なんわか: なん(PRON) + わ(ADP) + か(ADP) - "わ" is ADP!
 * - なんにんか: なんに(VERB) + ん(SCONJ) + か(ADP) - "なんに" is VERB!
 * - なんだいか: な(AUX) + ん(PRON) + だ(AUX) + いか(NOUN) - "か" embedded in "いか"! Unmatchable!
 * - なんかいか: なんかい(NOUN) + か(ADP) - compound without か!
 *
 * Due to GiNZA's extremely inconsistent and unpredictable tokenization,
 * we need multiple branches with flexible POS matching.
 */
export default linguisticRule('なん-counter-か', (r) => {
  r.either(
    // Branch 1: いくつか (some things/a few things)
    // This is a fixed expression, not followed by a counter
    (b1) => {
      const ikutsu = b1.tok({
        textOneOf: ['いくつ'],
      }, 'ikutsu');

      const ka = b1.tok({
        text: 'か',
        posOneOf: ['PART', 'ADP'],
      }, 'ka');

      b1.inOrder(ikutsu, ka, 1);
      b1.captureSpan('なん-counter-か', ikutsu, ka);
    },

    // Branch 2: 何/なん + counter (very flexible POS) + か
    // GiNZA tags counters as AUX, NOUN, ADP, or even VERB!
    (b2) => {
      const nani = b2.tok({
        textOneOf: ['何', 'なん', 'なに', 'なんに'],  // "なんに" for "なんにんか"
      }, 'nani');

      // Counter can be almost any POS (GiNZA is extremely inconsistent)
      const counter = b2.tok({
        posOneOf: ['AUX', 'NOUN', 'ADP', 'VERB', 'SCONJ'],
      }, 'counter');

      const ka = b2.tok({
        text: 'か',
        posOneOf: ['PART', 'ADP'],
      }, 'ka');

      b2.inOrder(nani, counter, 1);
      b2.inOrder(counter, ka, 1);
      b2.captureSpan('なん-counter-か', nani, ka);
    },

    // Branch 3: 幾/いく + counter (very flexible POS) + か
    (b3) => {
      const iku = b3.tok({
        textOneOf: ['幾', 'いく'],
      }, 'iku');

      const counter = b3.tok({
        posOneOf: ['AUX', 'NOUN', 'ADP', 'VERB', 'SCONJ'],
      }, 'counter');

      const ka = b3.tok({
        text: 'か',
        posOneOf: ['PART', 'ADP'],
      }, 'ka');

      b3.inOrder(iku, counter, 1);
      b3.inOrder(counter, ka, 1);
      b3.captureSpan('なん-counter-か', iku, ka);
    },

    // Branch 4: Compound form + か (e.g., なんかい + か)
    // GiNZA sometimes tokenizes 何+counter together but separates か
    (b4) => {
      // Specific number+counter compounds from test data
      const compound = b4.tok({
        textOneOf: [
          'なんかい',   // 何階 (floors)
          // Add more as needed
        ],
      }, 'compound');

      const ka = b4.tok({
        text: 'か',
        posOneOf: ['PART', 'ADP'],
      }, 'ka');

      b4.inOrder(compound, ka, 1);
      b4.captureSpan('なん-counter-か', compound, ka);
    }
  );
});
