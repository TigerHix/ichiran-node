import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: どうせ (douse) - "anyway, in any case, after all, at any rate"
 *
 * An adverb expressing resignation or acceptance of an inevitable outcome.
 * Often carries a nuance of frustration or pessimism about unavoidable
 * situations. Indicates that no matter what is done, the result will be
 * the same.
 *
 * Structures:
 * - どうせ + Phrase
 * - Can appear at beginning or middle of sentences
 *
 * Examples:
 * - どうせ失敗するに決まっている。
 *   (I have no doubt that I will fail anyway.)
 * - どうせ車を買うのなら、いいのを買おう。
 *   (If we're buying a car anyway, let's buy a good one.)
 * - どうせ彼は許すつもりはないだろう。
 *   (He probably won't forgive me anyway.)
 * - またどうせ散らかすんでしょ。
 *   (You'll just make a mess again anyway.)
 *
 * Key discriminators:
 * - Written as どうせ (hiragana)
 * - POS: ADV (adverb)
 * - Expresses inevitability or resignation
 *
 * GiNZA parse structure:
 * - どうせ (ADV, lemma=どうせ, dep=advmod)
 *
 * Different from:
 * - 何しろ (nanihiro) - explanatory, gives reasons
 * - 何といっても (nan to ittemo) - emphasizes quality/importance
 * - とにかく (tonikaku) - "anyway, in any case" (less emotional)
 * - やっぱり/やはり (yappari/yahari) - "as expected" (less defeatist)
 * - 絶対/絶対に (zettai/zettai ni) - "absolutely, certainly" (more emphatic)
 * - 必ず/必ずしも (kanarazu/kanarazushimo) - "certainly/not necessarily"
 * - きっと (kitto) - "surely, probably" (less certain)
 * - どう (dou) - "how" (interrogative adverb)
 */
export default linguisticRule('どうせ', (r) => {
  const douse = r.adv({
    lemma: 'どうせ',
  }, 'douse');

  r.capture(douse);
});
