import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ほとんど (殆ど) - Almost all / Mostly / Hardly any
 *
 * Matches ほとんど (hotondo) as an adverb/adverbial noun meaning "almost all",
 * "mostly", "nearly", or "hardly any" depending on whether the sentence is
 * positive or negative.
 *
 * Structures:
 * - ほとんど + Phrase (modifies the entire sentence/clause)
 * - ほとんど + の + Noun (most people, most things, etc.)
 *
 * Examples:
 * - ほとんどの人が帰りました (Almost everyone went home)
 * - 宿題はほとんどない (There's hardly any homework)
 * - ほとんど残っている (Almost all is left)
 * - ほとんど知っている (Know almost all of it)
 *
 * Key discriminators:
 * - POS can be ADV (adverb) or NOUN (can function as either in Japanese)
 * - textOneOf allows both hiragana (ほとんど) and kanji (殆ど) forms
 *
 * Note: Unlike あまり (which requires negative), ほとんど can be used with
 * both positive and negative predicates. In positive contexts it means "almost
 * all" or "mostly", while in negative contexts it means "hardly any".
 */
export default bunproLinguisticRule('ほとんど', (r) => {
  const hotondo = r.tok({
    posOneOf: ['ADV', 'NOUN', 'PRON'],
    textOneOf: ['ほとんど', '殆ど'],
  }, 'hotondo');

  // Capture the adverb itself
  r.capture(hotondo);
});
