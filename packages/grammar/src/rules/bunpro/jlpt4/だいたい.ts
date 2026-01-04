import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: だいたい (大体) - Generally / Mostly / Approximately
 *
 * Matches だいたい (daitai) as an adverb meaning "generally", "mostly",
 * "approximately", or "for the most part".
 *
 * Structures:
 * - だいたい + Phrase (modifies the entire sentence/clause)
 * - だいたい + Number (approximately X)
 * - だいたい + の + Noun (most people, most situations, etc.)
 *
 * Examples:
 * - 日本語が大体わかる (I roughly understand Japanese)
 * - 水曜日はだいたい５時に起きています (On Wednesdays, I generally wake up at 5)
 * - 大体３時間くらいでできます (It takes approximately 3 hours)
 * - 大体の人は食べたり寝たりするのが好きです (Most people enjoy eating and sleeping)
 * - 大体でいいから (Roughly is fine)
 * - 大体、太郎が一人でそこへ行ったことは間違いなんです (In the first place, it was a mistake...)
 *
 * Key discriminators:
 * - POS can be ADV (adverb) or NOUN (can function as either in Japanese)
 * - lemmaOneOf allows both hiragana (だいたい) and kanji (大体) forms
 *
 * Note: だいたい can also mean "in the first place" when used at the beginning
 * of a sentence (e.g., 大体、なんでお前がここにいるの？). This is captured
 * by the same rule since it's the same word with a different nuance.
 */
export default linguisticRule('だいたい', (r) => {
  const daitai = r.tok({
    posOneOf: ['ADV', 'NOUN', 'PRON'],  // PRON is used when sentence has context prefix
    textOneOf: ['だいたい', '大体'],
  }, 'daitai');

  // Capture the adverb itself
  r.capture(daitai);
});
