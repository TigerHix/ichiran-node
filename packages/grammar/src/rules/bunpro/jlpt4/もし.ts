import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: もし (if/in case) - Conditional emphasis marker
 *
 * Matches もし (moshi) as an adverb used to emphasize conditional/hypothetical statements.
 *
 * Structure:
 * - もし + Conditional phrase (たら, ば, と, なら, ても)
 *
 * Meaning: "If" / "In case" / "Supposing" - adds emphasis to hypothetical conditions
 *
 * Examples:
 * - もし雨が降ったら、車で行く。 (If it rains, I'll go by car.)
 * - もし時間があれば、来てください。 (If you have time, please come.)
 * - もし辛いものだったら、食べられない。 (If it's spicy, I can't eat it.)
 * - もしこのパソコンを彼女にあげるなら、説明書も渡してね。 (If you're going to give this computer to her, give her the manual too.)
 * - もし雨が降っても遊園地に行きます。 (Even if it rains, we'll go to the amusement park.)
 *
 * Key discriminators:
 * - POS is ADV (adverb)
 * - textOneOf allows both hiragana (もし) and kanji (若し) forms
 * - Typically appears at the beginning of a sentence or clause
 * - Almost always followed by a conditional form (たら, ば, と, なら, ても)
 *
 * Notes:
 * - もし is an adverb that emphasizes the hypothetical nature of a condition
 * - Similar to English "if" or "supposing that" placed at the start of a sentence
 * - Can be combined with various conditional endings
 * - もしも (moshimo) is a more emphatic variant (JLPT3 grammar point)
 */
export default linguisticRule('もし', (r) => {
  const moshi = r.adv({
    textOneOf: ['もし', '若し'],
  }, 'moshi');

  // Capture the adverb itself
  // Note: The following conditional (たら/ば/と/なら/ても) is not required
  // to match, as this rule just marks the presence of もし
  r.capture(moshi);
});
