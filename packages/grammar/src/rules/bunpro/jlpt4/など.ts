import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: など (nado) - Such as, Things like, Etc., And so on
 *
 * Matches the particle など used to give non-exhaustive examples.
 *
 * Structures:
 * - Noun + など + particle (が, を, に, etc.)
 * - Noun + や + Noun + など (A and B etc.)
 * - Noun + などの + Noun (things like A, in category B)
 *
 * Examples:
 * - 趣味などを教えてください (Please tell me things like your hobbies)
 * - フルーツはぶどうなどが好きです (As for fruits, I like things like grapes)
 * - イタリアやアメリカなどに行きたい (I want to go to places like Italy and America)
 * - カレーなどの辛いものが好きです (I like spicy things like curry)
 *
 * Key discriminators:
 * - など is an adverbial particle (pos=ADP in GiNZA, lemma=など)
 * - Can appear with various dependency relationships (case, obl, etc.)
 * - Must distinguish from similar particles like とか (more casual)
 *
 * GiNZA parse structure:
 * - POSITIVE: お土産などを買いました
 *   - 土産(NOUN) + など(ADP, lemma=など, dep=case) + を(PART)
 * - POSITIVE: 趣味などを教えてください
 *   - 趣味(NOUN) + など(ADP, lemma=など, dep=case) + を(PART)
 * - POSITIVE: ぶどうなどが好きです
 *   - ぶどう(NOUN) + など(ADP, lemma=など, dep=case) + が(PART)
 * - POSITIVE: イタリアやアメリカなどに行きたい
 *   - イタリア(NOUN) + や(PART) + アメリカ(NOUN) + など(ADP, lemma=など, dep=case) + に(PART)
 */
export default linguisticRule('など', (r) => {
  // など is parsed as ADP (adposition/particle) by GiNZA
  const nado = r.tok({
    lemma: 'など',
    pos: 'ADP'
  }, 'nado');

  // Capture the など particle
  r.capture(nado);
});
