import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: いきなり (ikinari) - "suddenly, all of a sudden, for no reason"
 *
 * An adverb expressing that something happens suddenly, abruptly, or without warning.
 * Can also imply "for no reason" or "without preparation" depending on context.
 *
 * Structures:
 * - いきなり + Verb/Adjective Phrase (suddenly/unexpectedly)
 * - 行き成り + Verb/Adjective Phrase (kanji form - rare)
 *
 * Examples:
 * - いきなり電話してごめん。
 *   (Sorry for calling you suddenly.)
 * - いきなり雨が降ってきたからびしょびしょだよ。
 *   (It suddenly started raining and I'm drenched.)
 * - ペットのワンちゃんがいきなり吠え出したからびっくりして起きた。
 *   (I was startled awake when my pet dog suddenly started barking.)
 * - いきなりの展開に驚きを隠せない。
 *   (I can't hide my surprise at the sudden development.)
 * - いきなり手続きはできません。
 *   (I cannot just all of a sudden process it.)
 *
 * Key discriminators:
 * - POS is ADV (adverb)
 * - text is 'いきなり' (most common hiragana form)
 * - Can also appear as kanji '行き成り' (rare)
 * - Different from similar adverbs:
 *   - 急に (kyuu ni) - suddenly, but more neutral/descriptive
 *   - 突然 (totsuzen) - suddenly, unexpectedly (more formal)
 *   - たちまち (tachimachi) - immediately, quickly (emphasizes speed)
 *
 * Note: いきなり emphasizes lack of preparation or warning. It comes from
 * 行き成り (development/unfolding) and implies skipping stages or order.
 * Almost always written in hiragana; kanji form is very rare.
 */
export default linguisticRule('いきなり', (r) => {
  r.either(
    // Branch 1: hiragana form いきなり (most common)
    (b) => {
      const ikinari = b.adv({
        text: 'いきなり',
      }, 'ikinari');
      b.capture(ikinari);
    },

    // Branch 2: kanji form 行き成り (rare variant)
    (b) => {
      const ikinariKanji = b.adv({
        text: '行き成り',
      }, 'ikinariKanji');
      b.capture(ikinariKanji);
    }
  );
});
