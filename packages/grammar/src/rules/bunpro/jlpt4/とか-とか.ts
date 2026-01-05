import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: とか-とか (things like A and B / etc.)
 *
 * Matches the pattern of listing examples with とか particle.
 * This is a non-exhaustive listing particle that can follow nouns, verbs, or adjectives.
 *
 * Structure:
 * - Noun/Verb/Adj + とか (+ Noun/Verb/Adj + とか)
 * - The second とか is optional - a single とか can mean "things like X, among others"
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
 * - 日本語で「th」の音とかはないです。
 *   (Japanese doesn't have a 'th' sound, among other things.)
 *
 * Key discriminators:
 * - Must have at least one item followed by とか
 * - Each とか must have dep=case (particle marking)
 * - This distinguishes from other とか usages (like quoting)
 *
 * GiNZA parse structure:
 * GiNZA tokenizes とか as TWO separate tokens:
 * - POSITIVE: 苺とか
 *   - 苺(NOUN) + と(ADP, lemma=と, dep=case, head→noun)
 *   - か(ADP/PART, lemma=か, dep=case/mark, head→noun)
 * - POSITIVE: 音とか
 *   - 音(NOUN) + と(ADP, lemma=と, dep=case, head→noun)
 *   - か(ADP, lemma=か, dep=case, head→noun)
 */
export default bunproLinguisticRule('とか-とか', (r) => {
  // Item + とか (split into と and か by GiNZA)
  // Can be NOUN, VERB, ADJ (い-adjectives), ADV, PRON, PROPN
  // ADV is included for words like "とんこつラーメン" that GiNZA parses as adverbial
  const item = r.tok({
    posOneOf: ['NOUN', 'VERB', 'ADJ', 'ADV', 'PRON', 'PROPN'],
  }, 'item');
  const to = r.tok({
    text: 'と',
    pos: 'ADP',
    dep: 'case',
  }, 'to');
  const ka = r.tok({
    text: 'か',
    posOneOf: ['ADP', 'PART'],
    depOneOf: ['case', 'mark'],
  }, 'ka');

  // Both particles must be children of the item
  r.caseMarker(item, to);
  // ka can be either 'case' or 'mark' dependency - we don't specify, just check it's a child
  r.headChild(item, ka);

  // Ensure proper ordering: item -> to -> ka (allow small gap for punctuation)
  r.inOrder(item, to, 1);
  r.inOrder(to, ka, 2);

  // Capture the item + とか pattern
  r.captureSpan('とか-とか', item, ka);
});
