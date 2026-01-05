import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: とっくに (tokkuni) - "long ago, already"
 *
 * An adverb expressing that something happened long ago or is already complete.
 * A stronger form of "もう" (mou) or "すでに" (sude ni), emphasizing that the
 * action or state occurred far in the past.
 *
 * Structures:
 * - とっくに + Phrase (at beginning of sentence or before predicate)
 * - 疾っくに + Phrase (kanji form - rare)
 *
 * Examples:
 * - とっくに食べちゃったよ。
 *   (I already ate [long ago].)
 * - とっくに過ぎているのでやってもやらなくても関係がない。
 *   (The due date has long since passed, so it won't matter if I do it or not.)
 * - みんなはもうとっくに帰ってるぞ！
 *   (Everyone has already gone home ages ago!)
 * - 彼はとっくに死んだ。10年前にね。
 *   (He died a long time ago. 10 years back, actually...)
 *
 * Key discriminators:
 * - Usually written in hiragana as とっくに
 * - Can be written as 疾っくに (kanji form, rare)
 * - Emphasizes completion in the distant past
 * - Stronger than もう or すでに
 *
 * GiNZA parse structure:
 * - とっく (ADverb) + に (ADP) - adverb + particle
 * - GiNZA tokenizes as two separate tokens
 * - Followed by verb, adjective, or modifies entire clause
 *
 * Different from:
 * - もう (mou) - "already" (neutral, everyday usage)
 * - すでに (sude ni) - "already" (more formal)
 * - とっくに has stronger emphasis on "long ago"
 * - とっくに often used for exaggeration
 */
export default linguisticRule('とっくに', (r) => {
  const tokku = r.tok({
    text: 'とっく',
  }, 'tokku');

  const ni = r.particle('に', 'ni');

  // とっく followed immediately by に
  r.inOrder(tokku, ni, 1);

  // Capture both tokens together as "とっくに"
  r.captureSpan('とっくに', tokku, ni);
});
