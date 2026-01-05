import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: いよいよ (iyoiyo) - "at last, finally, more and more"
 *
 * An adverb indicating that something is progressing or reaching a climax.
 * Used in two main contexts:
 *
 * 1. "Finally/at last" - indicating that a long-expected moment has arrived
 *    After much anticipation or effort, something is finally happening.
 *
 * 2. "More and more" - indicating increasing degree or intensity
 *    Something is compounding or getting stronger.
 *
 * Structures:
 * - いよいよ + Phrase (always at beginning of sentence/phrase)
 *
 * Examples:
 * - いよいよ明日で卒業か...。
 *   (Tomorrow is finally our graduation, isn't it...)
 * - いよいよ俺の番だ。緊張するけど頑張らなくちゃ！
 *   (It is finally my turn. I am nervous, but I must do my best!)
 * - 台風が接近するにつれて、いよいよ風が強くなってきた。
 *   (As the typhoon approached, the wind started to blow more and more.)
 * - 彼の話を聞いていると彼がいよいよ怪しくなってきた。
 *   (Listening to his story makes him more and more suspicious.)
 *
 * Key discriminators:
 * - Can be written as いよいよ (hiragana) or 愈々 (kanji)
 * - Always appears at the beginning of a sentence or clause
 * - POS is ADV (adverb)
 * - Modifies the entire following predicate
 *
 * Different from:
 * - やっと/ようやく (yatto/yoyaku) - "finally" with positive nuance, after effort
 * - ついに/遂に (tsui ni) - more formal "finally"
 * - とうとう (toutou) - "after all, finally" often with negative result
 * - ますます (masumasu) - "more and more" (only increasing, not climax)
 *
 * GiNZA parse structure:
 * - いよいよ/愈々 (ADV) - adverb
 * - Typically followed by:
 *   - Verb, adjective, or noun predicate
 *   - No particle required (direct adverbial modification)
 */
export default bunproLinguisticRule('いよいよ', (r) => {
  const iyoiyo = r.tok({
    lemmaOneOf: ['いよいよ', '愈々'],
    pos: 'ADV',
  }, 'iyoiyo');

  // いよいよ is a simple adverb that stands alone
  // It appears at the beginning of sentences and modifies the entire clause
  r.capture(iyoiyo);
});
