import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: とか (to ka) - I heard that... or something
 *
 * A casual hearsay expression that quotes something heard with uncertainty.
 * Similar to "I heard (A) or something" or "They say (A) or something like that."
 *
 * This is the QUOTATIONAL usage (combining quotative particle と + question particle か),
 * NOT the listing usage (examples like "X, Y, etc.").
 *
 * Structure:
 * - Verb (casual form) + とか
 * - I-adjective + とか
 * - Na-adjective + だ + とか
 * - Noun + だ + とか
 *
 * Examples:
 * - 遠藤さんと近藤さんが結婚するとか。
 *   (I heard Endo-san and Kondo-san are getting married or something.)
 * - 納豆は体にいいんだとか。
 *   (I heard Natto is good for your body or something.)
 * - 高橋くんの彼女が美人だとか。
 *   (I heard Takahashi's girlfriend is beautiful or something.)
 * - 先生がテストの問題は難しくないとか言ってたけど本当かな？
 *   (The teacher was saying the test questions weren't hard or something like that.)
 *
 * Key discriminators:
 * - Appears at end of sentence or before final punctuation
 * - Follows casual form of verb/adj/noun+だ
 * - Expresses uncertainty/heard information
 * - NOT followed by another noun (that would be listing usage)
 * - NOT the quotative particle と followed by question marker か separately
 *
 * GiNZA parse structure:
 * - とか often parsed as PART or ADP
 * - May have dep=mark (sentence-final particle)
 *
 * Different from:
 * - Listing とか (noun + とか + noun): "りんごとかバナナを買う" (buy things like apples and bananas)
 * - Simple question か: "行くか？" (Will you go?)
 * - Quotative と alone: "行くと言った" (said (they) will go)
 */
export default linguisticRule('とか', (r) => {
  // Main pattern: Match any predicate followed by とか
  // This catches the quotational/hearsay usage of とか
  const predicate = r.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON', 'AUX'] }, 'predicate');
  const toka = r.tok({ text: 'とか' }, 'toka');

  r.inOrder(predicate, toka, 30);
  r.captureSpan('とか', predicate, toka);
});
