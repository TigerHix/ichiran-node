import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: いつの間にか (itsunomanika) - "before one knew it, unnoticed, suddenly"
 *
 * An adverbial phrase meaning "before one knew it," "unnoticed," or "without realizing."
 * Expresses that something happened without the speaker noticing when or how it occurred.
 * Carries a nuance of surprise that something changed during an unknown interval.
 *
 * Structures:
 * - いつの間にか + Phrase
 * - いつのまにか + Phrase (hiragana variant)
 *
 * Examples:
 * - いつの間にかもう一年が経ってしまった。
 *   (Without me realizing it, another year has passed.)
 * - 気づいたら、いつの間にか冬になっていた。
 *   (Before I knew it, it was winter.)
 * - 子供の頃は、いつの間にか日が沈みかけていることも気付かずに、外で夢中になって遊んだ。
 *   (When I was a child, without noticing that the sun was about to set, I played outside completely absorbed.)
 *
 * Key discriminators:
 * - Can be written as いつの間にか (mixed kanji/hiragana) or いつのまにか (all hiragana)
 * - Functions as an adverb (ADV)
 * - Always ends with か (question particle creating indefinite sense)
 * - Emphasizes the speaker's lack of awareness during the change
 * - Different from あっという間に (instant speed) and 突然 (abruptness)
 *
 * GiNZA parse structure:
 * - いつの間にか (ADV) - parsed as a compound adverb
 * - Note: "の間" (no ma) may be analyzed as NOUN + case marker
 * - The final "か" is part of the compound adverb structure
 *
 * Different from:
 * - あっという間に (atto iu ma ni) - "in a flash" (emphasizes speed, not lack of awareness)
 * - 突然/いきなり (totsuzen/ikinari) - "suddenly" (emphasizes abruptness, not unnoticed change)
 * - たちまち (tachimachi) - "immediately" (emphasizes spontaneity and immediacy)
 * - つい (tui) - "unintentionally" (emphasizes lack of control, not passage of time)
 */
export default bunproLinguisticRule('いつの間にか', (r) => {
  // いつの間にか (before one knew it, unnoticed, suddenly)
  // Adverbial phrase meaning something happened without the speaker noticing
  // Can be written with kanji (いつの間にか) or all hiragana (いつのまにか)
  // GiNZA typically parses this as a compound ADV token
  // Structure: いつ (when) + の (possessive) + 間/ま (interval) + に (particle) + か (question particle)

  r.either(
    // Pattern 1: Full compound adverb (most common in GiNZA)
    // GiNZA parses the complete phrase as a single ADV token
    (b1) => {
      const itsunomanika = b1.tok({
        lemmaOneOf: ['いつの間にか', 'いつのまにか'],
        pos: 'ADV',
      }, 'itsunomanika');

      b1.capture(itsunomanika);
    },

    // Pattern 2: Multi-token analysis
    // If GiNZA parses it as separate tokens: いつ + の + 間/ま + に + か
    (b2) => {
      const itsu = b2.tok({ lemma: 'いつ' }, 'itsu');
      const no = b2.particle('の', 'no');
      const ma = b2.tok({
        lemmaOneOf: ['間', 'ま'],
        posOneOf: ['NOUN', 'ADV'],
      }, 'ma');
      const ni = b2.particle('に', 'ni');
      const ka = b2.particle('か', 'ka');

      b2.inOrder(itsu, no, 1);
      b2.inOrder(no, ma, 1);
      b2.inOrder(ma, ni, 1);
      b2.inOrder(ni, ka, 1);
      b2.captureSpan('いつの間にか', itsu, ka);
    }
  );
});
