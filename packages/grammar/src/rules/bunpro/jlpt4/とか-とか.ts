import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: とか-とか (things like A and B / etc.)
 *
 * Matches the pattern of listing examples with とか particle.
 * This is a non-exhaustive listing particle that can follow nouns, verbs, or adjectives.
 *
 * Structure:
 * - Noun/Verb/Adj + とか + Noun/Verb/Adj + とか (at least two occurrences)
 *
 * The particle とか is used to give examples of things or actions, implying that
 * there are other examples not mentioned. It's more casual than similar particles
 * like や and can be used with verbs (unlike や).
 *
 * Examples:
 * - 苺とか、りんごとかは嫌いです。
 *   (I don't like things like strawberries and apples, among others.)
 * - 野菜を食べるとか水を飲むとかをしています。
 *   (I do things like eating vegetables and drinking water.)
 * - 掃除とか、料理とか、何もしていない！
 *   (I haven't done anything like cleaning or cooking!)
 *
 * Key discriminators:
 * - Must have at least two items followed by とか
 * - Each とか must have dep=case (particle marking)
 * - This distinguishes from single とか usages
 *
 * GiNZA parse structure:
 * - POSITIVE: 苺とか、りんごとか
 *   - 苺(NOUN) + とか(PART, lemma=とか, dep=case, head→noun)
 *   - りんご(NOUN) + とか(PART, lemma=とか, dep=case, head→noun)
 * - POSITIVE: 食べるとか、飲むとか
 *   - 食べる(VERB) + とか(PART, lemma=とか, dep=case)
 *   - 飲む(VERB) + とか(PART, lemma=とか, dep=case)
 */
export default linguisticRule('とか-とか', (r) => {
  // First item + とか
  // Can be NOUN, VERB, or ADJ (covers い-adjectives)
  const item1 = r.tok({
    posOneOf: ['NOUN', 'VERB', 'ADJ', 'PRON', 'PROPN'],
  }, 'item1');
  const toka1 = r.tok({
    text: 'とか',
    pos: 'PART',
    dep: 'case',
  }, 'toka1');
  r.caseMarker(item1, toka1);
  r.inOrder(item1, toka1, 1);

  // Second item + とか (same pattern as first)
  const item2 = r.tok({
    posOneOf: ['NOUN', 'VERB', 'ADJ', 'PRON', 'PROPN'],
  }, 'item2');
  const toka2 = r.tok({
    text: 'とか',
    pos: 'PART',
    dep: 'case',
  }, 'toka2');
  r.caseMarker(item2, toka2);
  r.inOrder(item2, toka2, 1);

  // The second pair must come after the first (with reasonable distance for commas/particles)
  r.inOrder(toka1, item2, 10);

  // Capture from the first item through the last とか
  r.captureSpan('とか-とか', item1, toka2);
});
