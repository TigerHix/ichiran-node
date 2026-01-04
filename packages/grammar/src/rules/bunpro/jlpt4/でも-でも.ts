import { linguisticRule } from '../../../engine/lang.js';

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
 *
 * Note: The rule uses optional() to capture 3+ alternatives like AでもBでもCでも
 */
export default linguisticRule('でも-でも', (r) => {
  // First noun + で + も pair
  const noun1 = r.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun1');
  const de1 = r.tok({ textOneOf: ['で', 'だ'], posOneOf: ['ADP', 'AUX'], depOneOf: ['case', 'cop'] }, 'de1');
  const mo1 = r.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo1');
  r.caseMarker(noun1, de1);
  r.caseMarker(noun1, mo1);
  r.inOrder(noun1, de1, 1);
  r.inOrder(de1, mo1, 1);

  // Second noun + で + も pair
  const noun2 = r.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun2');
  const de2 = r.tok({ textOneOf: ['で', 'だ'], posOneOf: ['ADP', 'AUX'], depOneOf: ['case', 'cop'] }, 'de2');
  const mo2 = r.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo2');
  r.caseMarker(noun2, de2);
  r.caseMarker(noun2, mo2);
  r.inOrder(noun2, de2, 1);
  r.inOrder(de2, mo2, 1);

  // Must appear in order with reasonable distance (allow for commas)
  r.inOrder(mo1, noun2, 10);

  // Optional third pair (for patterns like AでもBでもCでも)
  r.optional((b) => {
    const noun3 = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun3');
    const de3 = b.tok({ textOneOf: ['で', 'だ'], posOneOf: ['ADP', 'AUX'], depOneOf: ['case', 'cop'] }, 'de3');
    const mo3 = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo3');
    b.caseMarker(noun3, de3);
    b.caseMarker(noun3, mo3);
    b.inOrder(noun3, de3, 1);
    b.inOrder(de3, mo3, 1);
    b.inOrder(mo2, noun3, 10);

    // Optional fourth pair (for longer lists)
    b.optional((c) => {
      const noun4 = c.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun4');
      const de4 = c.tok({ textOneOf: ['で', 'だ'], posOneOf: ['ADP', 'AUX'], depOneOf: ['case', 'cop'] }, 'de4');
      const mo4 = c.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo4');
      c.caseMarker(noun4, de4);
      c.caseMarker(noun4, mo4);
      c.inOrder(noun4, de4, 1);
      c.inOrder(de4, mo4, 1);
      c.inOrder(mo3, noun4, 10);
    });
  });

  // Capture from first noun through the last も we matched
  r.captureSpan('でも-でも', noun1, mo2);
});
