import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: でも - Or something / Any... (with question words)
 *
 * Matches でも when used as a particle meaning "or something" or "any..."
 * when attached to question words (だれ、どこ、何、いつ) or regular nouns.
 *
 * Structure:
 * - Noun/Pronoun + でも (particle)
 *
 * This is NOT the conjunction でも (but/however) that appears at sentence start.
 *
 * Examples:
 * - どこでもいいよ。 (Anywhere is fine.)
 * - お茶でもどう？ (How about tea or something?)
 * - 誰でも知っている。 (Everyone knows / Anyone knows.)
 * - いつでも電話してね。 (Call anytime.)
 *
 * Key discriminators:
 * - で must have dep=case (case marker) to distinguish from conjunction でも
 * - The preceding word can be any noun or pronoun
 * - This rule does NOT match sentence-initial でも (conjunction "but/however")
 *
 * GiNZA parse structure:
 * - お茶でも: お茶(NOUN) + で(ADP,dep=case) + も(ADP,dep=case)
 * - どこでも: どこ(PRON) + で(ADP,dep=case) + も(ADP,dep=case)
 *
 * Negative cases (should NOT match):
 * - でも、高いから買えない。 (Sentence-initial conjunction "but")
 * - 東京でも雨が降っている。 (Locative "even in Tokyo" - different grammar)
 */
export default bunproLinguisticRule('でも', (r) => {
  // Match で (particle, case marker)
  const de = r.tok({
    text: 'で',
    dep: 'case',
  }, 'de');

  // Followed by も (particle, case marker)
  const mo = r.tok({
    text: 'も',
    dep: 'case',
  }, 'mo');
  r.inOrder(de, mo, 1);

  // Capture the full expression
  r.captureSpan('でも', de, mo);
});
