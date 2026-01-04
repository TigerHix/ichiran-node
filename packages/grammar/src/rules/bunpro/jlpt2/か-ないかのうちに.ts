import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: か〜ないかのうちに (ka-naika-no-uchini) - As soon as, Just when, Right after
 *
 * A formal grammar point indicating that (B) happens at the precise moment when it's
 * uncertain whether (A) has finished or is still ongoing. The same verb must be repeated.
 *
 * Structure:
 * - Verb［る］+ か + Verb［ない］+ かのうちに
 *
 * The same verb must be repeated in both dictionary form and negative form.
 *
 * Examples:
 * - 家の戸を開けるか開けないかのうちに犬が飛び出してきた。
 *   (As soon as I opened the door, the dog jumped out.)
 * - 試験の答案を書き終えるか終えないかのうちに、火災警報が鳴った。
 *   (Just when I was about to finish the test, the fire alarm rang.)
 * - 彼は椅子に座るか座らないかのうちに、テレビをつけた。
 *   (He turned on the TV just as he sat down.)
 *
 * Key discriminators:
 * - Same verb appears twice: dictionary form + negative form (nai)
 * - Pattern: V-dict + か + V-nai + か + の + うち + に
 * - GiNZA parses negative verb with auxiliary ない attached
 *
 * GiNZA parse structure:
 * - 開けるか開けないかのうちに:
 *   - 開ける(VERB) + か(ADP) + 開け(VERB) + ない(AUX) + か(ADP) + の(SCONJ) + うち(NOUN) + に(ADP)
 */
export default linguisticRule('か-ないかのうちに', (r) => {
  // Pattern: Verb + か + Verb-stem + ない + か + の + うち + に
  // The two verbs should be the same, but in different forms (dictionary vs negative)

  // First verb: dictionary form - explicitly exclude "ない" from being matched
  const verb1 = r.tok({
    posOneOf: ['VERB', 'AUX'],
    textRe: /^(?!ない).+$/ // Negative lookahead to exclude "ない"
  }, 'verb1');

  // First particle: か (must come immediately after verb1)
  const ka1 = r.particle('か', 'ka1');
  r.inOrder(verb1, ka1, 1);

  // Second verb: in stem/negative form (before ない)
  const verb2 = r.tok({
    posOneOf: ['VERB', 'AUX'],
    textRe: /^(?!ない).+$/ // Also exclude "ない"
  }, 'verb2');

  // Negative auxiliary ない
  const nai = r.aux({ lemma: 'ない' }, 'nai');

  // verb2 should come immediately before nai
  r.inOrder(verb2, nai, 1);

  // ka1 comes before verb2 (with some distance for compound verbs)
  r.inOrder(ka1, verb2, 4);

  // Second particle: か (after nai)
  const ka2 = r.particle('か', 'ka2');
  r.inOrder(nai, ka2, 1);

  // の (conjunction/complementizer) - can be SCONJ or ADP
  const no = r.tok({ text: 'の', posOneOf: ['SCONJ', 'ADP'] }, 'no');
  r.inOrder(ka2, no, 1);

  // うち (noun: inside/within)
  const uchi = r.noun({ lemma: 'うち' }, 'uchi');
  r.inOrder(no, uchi, 2);

  // Final particle: に
  const ni = r.particle('に', 'ni');
  r.inOrder(uchi, ni, 1);

  // Capture the entire span from first verb to final particle
  r.captureSpan('か-ないかのうちに', verb1, ni);
});
