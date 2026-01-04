import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: いつの間にか (itsunomanika) - "before one knew it, unnoticed, without noticing"
 *
 * A fixed adverbial expression meaning something happened without the speaker
 * noticing or realizing it. Indicates an action or change that occurred during
 * an unknown or unnoticed time interval.
 *
 * Structure:
 * - いつの間にか + Phrase
 *
 * The expression consists of:
 * - いつ (itsu) - when
 * - の (no) - possessive particle
 * - 間 (ma) - interval/time
 * - に (ni) - time particle
 * - か (ka) - question particle (indicates uncertainty)
 *
 * Literally: "at some unknown time interval"
 *
 * Examples:
 * - いつの間にか日本語が上手になっていた。
 *   (Before I knew it, my Japanese had improved.)
 * - 気づいたら、いつの間にか冬になっていた。
 *   (When I realized it, it had become winter without me noticing.)
 * - いつの間にかもう１年が経ってしまった。
 *   (Another year has passed without me realizing it.)
 *
 * Key discriminators:
 * - Fixed expression written as いつの間にか (hiragana) or いつの間にか (mixed kanji)
 * - Functions as an adverb (ADV in GiNZA)
 * - Indicates unnoticed change or action
 * - Similar to あっという間に (attonomani) but less emphasis on speed, more on lack of awareness
 *
 * GiNZA parse structure:
 * - Parsed as a single ADV token: いつの間にか
 * - May be followed by various predicates (verb, adjective, noun sentences)
 *
 * Different from:
 * - あっという間に (attonomani) - "in a flash" (emphasizes speed, not lack of awareness)
 * - つい (tui) - "inadvertently" (focuses on unintentional action, not passage of time)
 * - 突然/急に (totsuzen/kyuuni) - "suddenly" (emphasizes suddenness, not unnoticed change)
 */
export default linguisticRule('いつの間にか', (r) => {
  // Match the fixed adverbial expression
  const itsunomanika = r.tok({
    textOneOf: ['いつの間にか', 'いつのまにか'],
    pos: 'ADV',
  }, 'itsunomanika');

  // Capture the entire expression
  r.capture(itsunomanika);
});
