import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: また (又) - Also, As well, Moreover, Again, Additionally
 *
 * Matches また (mata) as an adverb or conjunction meaning "again", "also",
 * "additionally", or "moreover".
 *
 * Structures:
 * - また + Verb/Adjective (again/also doing X)
 * - また、... (conjunction: moreover/additionally at sentence start)
 * - ...はまた... (in the middle of sentences)
 *
 * Examples:
 * - また寝坊したの？ (Did you oversleep again?)
 * - また遊ぼうね！ (Let's hang out again!)
 * - また、彼からチョコをもらった。 (Moreover, I received chocolate from him.)
 * - 彼は宇宙飛行士であり、またアナウンサーでもある。 (He is an astronaut, and also an announcer!)
 *
 * Key discriminators:
 * - POS is ADV (adverb)
 * - textOneOf allows both hiragana (また) and kanji (又) forms
 *
 * Note: また can be used as both an adverb and a conjunction. When used as
 * a conjunction, it typically appears at the beginning of a sentence or clause
 * and means "moreover" or "additionally". When used as an adverb in the middle
 * of a sentence, it typically means "again" or "also".
 */
export default bunproLinguisticRule('また', (r) => {
  const mata = r.tok({
    textOneOf: ['また', '又'],
    posOneOf: ['ADV', 'CCONJ'],  // ADV when used as adverb, CCONJ when used as conjunction
  }, 'mata');

  // Capture the adverb/conjunction itself
  r.capture(mata);
});
