import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: でも-でも (Noun + でも + Noun + でも) - Whether ~ or, Neither ~ or
 *
 * Matches the pattern of listing alternatives with でも particle, meaning "whether A or B" or "any of A, B, C".
 *
 * Structure:
 * - Noun + で + も + Noun + で + も (at least two occurrences)
 *
 * This pattern lists multiple alternatives and suggests that the result or evaluation doesn't change
 * regardless of which option is chosen. It's typically translated as "whether it's A or B" or
 * "any of A, B, or C".
 *
 * Examples:
 * - サッカーでもバスケットボールでもいいからスポーツをやりたい。
 *   (Whether it's soccer or basketball, I want to play sports.)
 * - 電車でもバスでも行ける場所なので、便利です。
 *   (You can get there whether by train or bus, so it's convenient.)
 * - 水でもお茶でも飲みたいな。
 *   (Whether it's water or tea, I want to drink it.)
 * - これでも、それでも、何でもいいです！
 *   (Whether this, that, or whatever, anything is fine!)
 *
 * Key discriminators:
 * - Each で must have dep=case or dep=cop (GiNZA inconsistency)
 * - Each も must have dep=case (emphatic particle attached to noun)
 * - Must have at least two noun+でも pairs
 * - This distinguishes from single でも usages and conjunction それでも (dep=fixed)
 *
 * GiNZA parse structure:
 * - POSITIVE: サッカーでもバスケットボールでも
 *   - サッカー(NOUN, dep=obl) + で(ADP, lemma=で, dep=case, head=0→NOUN) + も(ADP, lemma=も, dep=case, head=0→NOUN)
 *   - バスケットボール(NOUN, dep=obl) + で(ADP, lemma=で, dep=case, head=3→NOUN) + も(ADP, lemma=も, dep=case, head=3→NOUN)
 * - POSITIVE: 水でも、お茶でも (GiNZA parses inconsistently)
 *   - 水(NOUN) + で(ADP, lemma=で, dep=cop, head=0→NOUN) + も(ADP, lemma=も, dep=case, head=0→NOUN)
 *   - お茶(NOUN) + で(ADP, lemma=で, dep=case, head=3→NOUN) + も(ADP, lemma=も, dep=case, head=3→NOUN)
 * - POSITIVE: 円でも、ドルでも (GiNZA copula issue)
 *   - 円(NOUN) + で(ADP, lemma=で, dep=case, head=0→NOUN) + も(ADP, lemma=も, dep=case, head=0→NOUN)
 *   - ドル(NOUN) + だ(AUX, lemma=だ, dep=case, head=1→NOUN) + も(ADP, lemma=も, dep=case, head=1→NOUN)
 * - NEGATIVE (それでも): それでも行きます
 *   - それ(CCONJ,dep=cc) + で(ADP,dep=fixed) + も(ADP,dep=fixed)
 */
export default bunproLinguisticRule('でも-でも', (r) => {
  // First noun + で + も pair
  // Include ADJ for loanwords like ドル that GiNZA tags as adjectives
  const noun1 = r.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'ADJ'] }, 'noun1');
  const mo1 = r.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo1');
  r.caseMarker(noun1, mo1);

  // Handle both dep=case and dep=cop for で
  r.either(
    // dep=case: use caseMarker
    (b) => {
      const de1 = b.tok({ textOneOf: ['で', 'だ'], posOneOf: ['ADP', 'AUX'], dep: 'case' }, 'de1');
      b.caseMarker(noun1, de1);
      b.inOrder(noun1, de1, 1);
      b.inOrder(de1, mo1, 1);
      b.captureAs('de1', de1);
    },
    // dep=cop: use headChild
    (b) => {
      const de1 = b.tok({ textOneOf: ['で', 'だ'], posOneOf: ['ADP', 'AUX'], dep: 'cop' }, 'de1');
      b.headChild(noun1, de1, 'cop');
      b.inOrder(noun1, de1, 1);
      b.inOrder(de1, mo1, 1);
      b.captureAs('de1', de1);
    }
  );

  // Second noun + で + も pair
  const noun2 = r.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'ADJ'] }, 'noun2');
  const mo2 = r.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo2');
  r.caseMarker(noun2, mo2);

  // Handle both dep=case and dep=cop for で
  r.either(
    // dep=case: use caseMarker
    (b) => {
      const de2 = b.tok({ textOneOf: ['で', 'だ'], posOneOf: ['ADP', 'AUX'], dep: 'case' }, 'de2');
      b.caseMarker(noun2, de2);
      b.inOrder(noun2, de2, 1);
      b.inOrder(de2, mo2, 1);
      b.captureAs('de2', de2);
    },
    // dep=cop: use headChild
    (b) => {
      const de2 = b.tok({ textOneOf: ['で', 'だ'], posOneOf: ['ADP', 'AUX'], dep: 'cop' }, 'de2');
      b.headChild(noun2, de2, 'cop');
      b.inOrder(noun2, de2, 1);
      b.inOrder(de2, mo2, 1);
      b.captureAs('de2', de2);
    }
  );

  // The second pair must come after the first (with reasonable distance for commas)
  // Require de2 to come after mo1 to ensure proper ordering
  r.inOrder(mo1, noun2, 10);
  r.inOrder(mo1, mo2, 10);

  // Capture from the first noun through the last も
  r.captureSpan('でも-でも', noun1, mo2);
});
