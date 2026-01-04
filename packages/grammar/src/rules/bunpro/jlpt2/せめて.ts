import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: せめて (semete) - "at least, at the very least"
 *
 * An adverb indicating the minimum expected or acceptable outcome in a
 * less-than-ideal situation. Often expresses a sense of resignation,
 * criticism, or minimal hope. Originates from the verb 責める (semeru),
 * carrying a nuance of obligation or responsibility.
 *
 * Structures:
 * - せめて + Phrase (modifies entire sentence)
 * - せめて + Noun + だけ (at least X)
 * - せめて + Noun + くらい (at least X)
 * - Often used with: たい, ほしい, てほしい, べき (desire/opinion)
 *
 * Examples:
 * - せめてこの曲だけでも弾けるようになりたい。
 *   (I want to be able to play this song, at least.)
 * - せめておかずだけでも食べて。
 *   (At least eat your sides or something.)
 * - せめて1日は休みたい。
 *   (I want to take at least one day off.)
 * - せめてドアくらい閉めてほしかった。
 *   (I would have liked them to at least close the door.)
 *
 * Key discriminators:
 * - Always written as せめて (hiragana only)
 * - POS: ADV (adverb)
 * - Usually appears at the beginning of a sentence or clause
 * - Modifies the entire sentence, not just a following word
 * - Expresses minimal expectation in unfavorable situation
 *
 * GiNZA parse structure:
 * - せめて (ADV) - adverb
 * - May modify following phrase (advmod dependency)
 * - Often sentence-initial
 *
 * Different from:
 * - 少なくとも (sukunakutomo) - more neutral/objective "at least"
 * - だけ (dake) - "only/just" without the nuance of minimal expectation
 * - 最低限 (saiteigen) - "minimum/lowest limit" (more formal)
 */
export default linguisticRule('せめて', (r) => {
  const semete = r.adv({
    text: 'せめて',
    lemma: 'せめて',
  }, 'semete');

  r.capture(semete);
});
