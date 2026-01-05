import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: なるべく - As much as possible / Whenever practicable
 *
 * Matches なるべく (narubeku) as an adverb meaning "as much as possible" or "whenever possible".
 *
 * Structure:
 * - なるべく + Verb/Adjective/Phrase (do X as much as possible)
 *
 * Examples:
 * - なるべく早く行きます (go as fast as possible)
 * - なるべくわかりやすく説明してください (please explain as easy to understand as possible)
 * - なるべく沢山の人気な場所へ行きたい (want to go to as many famous places as possible)
 * - なるべくお金を貯めた方がいい (should save as much money as possible)
 * - なるべく綺麗にしてください (please make as clean as possible)
 *
 * Key discriminators:
 * - POS is ADV (adverb) OR compound of なる (verb) + べく (auxiliary)
 * - text is 'なるべく' (hiragana form, though '成る可く' is the kanji form)
 *
 * Note: Similar to できるだけ (dekirudake) which means the same thing but is more
 * direct/forceful. なるべく literally combines "to become" (なる) and "permissible" (可く)
 * to mean "whenever an opportunity presents itself", making it softer and more polite.
 *
 * GiNZA parses this inconsistently:
 * - Sometimes as single token: なるべく (ADV)
 * - Sometimes as two tokens: なる (VERB) + べく (AUX)
 * - We handle both cases with r.either()
 */
export default bunproLinguisticRule('なるべく', (r) => {
  // Variant 1: Single adverb token (most common)
  // GiNZA sometimes parses なるべく as a single adverb
  r.either((b) => {
    const narubeku = b.adv({
      text: 'なるべく',
    }, 'narubeku');

    // Capture the adverb itself
    b.capture(narubeku);
  },

  // Variant 2: Compound of なる + べく (GiNZA inconsistency)
  // Sometimes parsed as verb なる + auxiliary べく (from べし)
  (b) => {
    const naru = b.verb({
      lemma: 'なる',
    }, 'naru');

    const beku = b.aux({
      lemma: 'べし',
      text: 'べく',
    }, 'beku');

    // Require なる and べく to be adjacent
    b.inOrder(naru, beku, 1);

    // Capture the span from なる to べく
    b.captureSpan('なるべく', naru, beku);
  });
});
