import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ら (ra) - Casual plural marker suffix
 *
 * Matches the suffix ら used to indicate plurality for pronouns and nouns.
 * Similar to たち but more casual.
 *
 * Structures:
 * - Pronoun + ら (they, them, we, us)
 * - Demonstrative + ら (these, those)
 *
 * Examples:
 * - 彼らは日本語を勉強しにきた (They came to study Japanese)
 * - 君ら池に入るつもり？ (Do you guys intend to go in the pond?)
 * - 僕らは全員日本に留学したことがあります (We have all studied abroad in Japan)
 * - これらを捨ててください (Please throw these away)
 * - それらの畳はいい匂いがしている (Those tatami mats smell good)
 *
 * Key discriminators:
 * - ら is a noun suffix indicating plurality
 * - Attaches to pronouns (彼, 君, 僕, etc.) and demonstratives (これ, それ)
 * - More casual than たち
 *
 * GiNZA parse structure:
 * - POSITIVE: 彼らは日本語を勉強しにきた
 *   - 彼ら(NOUN or PRON) + は(PART)
 * - POSITIVE: 僕らは全員日本に留学した
 *   - 僕ら(NOUN or PRON) + は(PART)
 * - POSITIVE: これらを捨ててください
 *   - これら(NOUN or PRON) + を(PART)
 */
export default linguisticRule('ら', (r) => {
  // ら is a suffix that attaches to pronouns/demonstratives
  // In GiNZA, the base word+ら is often parsed as a single token
  const ra = r.tok({
    text: 'ら',
  }, 'ra');

  // Capture the ら suffix
  r.capture(ra);
});
