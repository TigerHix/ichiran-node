import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: どんどん - Progressively / Rapidly increasing / More and more
 *
 * Matches どんどん (dondon) as an adverb expressing rapid progression.
 *
 * Structures:
 * - どんどん + Verb/Adjective (rapidly becomes/does X)
 * - どんどんと + Verb/Adjective (same meaning, particle と is optional)
 *
 * Examples:
 * - どんどん暑くなる (rapidly getting hotter)
 * - どんどん大きくなる (rapidly getting bigger)
 * - どんどん日本語が上手になりたい (want to get better and better at Japanese)
 * - どんどんと減っている (rapidly decreasing)
 *
 * Key discriminators:
 * - POS is ADV (adverb)
 * - text is 'どんどん' (hiragana form)
 * - The optional particle と can follow どんどん
 *
 * Note: Similar to だんだん (gradual progression) but どんどん expresses
 * faster, more dynamic change - something happening rapidly or in leaps and bounds.
 */
export default linguisticRule('どんどん', (r) => {
  const dondon = r.adv({
    text: 'どんどん',
  }, 'dondon');

  // Capture the adverb itself
  // Note: The particle と is optional and not part of the grammar rule
  r.capture(dondon);
});
