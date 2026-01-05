import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: せめて (semete) - "at least, at the very least"
 *
 * An adverb indicating a minimum expectation or requirement. Used when
 * something cannot be done to an acceptable degree, but at least (A)
 * can be done. Expresses a minimal hope, consolation, or sometimes
 * criticism/obligation in an otherwise unfavorable situation.
 *
 * Structures:
 * - せめて + Phrase (usually at sentence start)
 *
 * Examples:
 * - せめてドアくらい閉めてほしい。
 *   (I would have liked them to at least close the door.)
 * - ご飯は残しても良いから、せめて刺身だけは全部たべて。
 *   (You can leave the rice, but at least finish the sashimi please.)
 * - せめて国語の宿題はやっておきなさい。
 *   (At least finish your Japanese homework in advance.)
 *
 * Key discriminators:
 * - Written as せめて (hiragana)
 * - Functions as an adverb (ADV) indicating minimum expectation
 * - Usually appears at sentence start, modifies entire sentence
 * - Often used with expressions of desire/opinion: たい, ほしい, てほしい, べき
 * - Different from 少なくとも (sukunakutomo) - more formal/neutral
 *
 * GiNZA parse structure:
 * - せめて (ADV) - adverb
 *
 * Different from:
 * - 少なくとも (sukunakutomo) - "at least" (more formal, neutral)
 * - どうせ (douse) - "anyway, in any case" (resignation)
 * - どうやら (douyara) - "apparently, seemingly" (conjecture)
 */
export default linguisticRule('せめて', (r) => {
  // せめて (at least, at the very least)
  // Adverb indicating minimum expectation or requirement
  // Usually at sentence start, modifying the entire sentence
  // GiNZA tags this as ADV (adverb)
  // Often used with desire/opinion markers (たい, ほしい, てほしい, べき)

  const semete = r.tok({
    lemma: 'せめて',
    pos: 'ADV',
  }, 'semete');

  // Capture just the adverb itself
  // It modifies whatever follows in the sentence
  r.capture(semete);
});
