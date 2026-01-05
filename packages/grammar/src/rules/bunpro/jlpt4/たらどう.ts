import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: たらどう - Why don't you...? / What if you did...?
 *
 * Matches verb ta-form + らどう pattern used for making casual suggestions.
 *
 * Pattern: Verb［た-form］+ ら + どう
 *
 * This is a casual, sometimes sarcastic way to suggest that someone do something.
 * It translates to "why don't you..." or "what if you did...?"
 *
 * Examples:
 * - ひろったらどうですか (Why don't you pick it up?)
 * - してみたらどう (Why don't you try doing it?)
 * - かえったらどう (How about going back?)
 *
 * GiNZA parse structure:
 * - Verb stem + auxiliary with text ending in "たら"
 * - The "たら" conditional is parsed as a single token (AUX or SCONJ)
 * - Followed by どう (adverb meaning "how")
 *
 * Key discriminators:
 * - Must have a verb followed by an auxiliary ending in "たら"
 * - Must be followed by どう (adverb)
 */
export default bunproLinguisticRule('たらどう', (r) => {
  // Any verb that can be conjugated
  const verb = r.verb({}, 'verb');

  // Auxiliary that ends in "たら" - this captures the ta-form + conditional
  // This matches tokens like "たら", "ったら" which are ta-form + ra
  const tara = r.tok({
    textOneOf: ['たら', 'ったら', 'だら'],
    posOneOf: ['AUX', 'SCONJ'],
  }, 'tara');

  // The auxiliary must attach to the verb
  r.auxOf(verb, tara);

  // どう (adverb "how")
  const dou = r.adv({
    text: 'どう',
    lemma: 'どう',
  }, 'dou');

  // Require sequence: verb-tara-dou (with some gap allowed)
  r.inOrder(tara, dou, 3);

  // Capture from verb to どう
  r.captureSpan('たらどう', verb, dou);
});
