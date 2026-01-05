import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: その上 (sono ue) - "moreover, on top of that, in addition"
 *
 * A formal conjunction/adverb used to add information to a previous statement.
 * Literally means "on top of that" and is used to introduce additional points
 * or information that builds upon what was previously mentioned.
 *
 * Structures:
 * - [Statement A]. その上、[Statement B].
 * - [Statement A]。その上、[Statement B].
 *
 * Examples:
 * - 高橋さんはとても頭がいい。その上、人柄もいいので会社での評判がいいです。
 *   (Takahashi-san is very smart. Furthermore, he has a good personality so he has a good reputation in the company.)
 * - 顔が青白い。その上、唇は紫だ。大丈夫だろうか？
 *   (His face is pale. Furthermore, his lips are purple. Is he okay, I wonder?)
 * - あの店は安い。その上回転も早くてすぐに座れるから、気軽に行ける。
 *   (That shop is cheap. In addition, the turnaround is quick and you can get a seat right away.)
 *
 * Key characteristics:
 * - Formal register (硬い)
 * - Used at the beginning of sentences
 * - Adds information in a neutral, polite way
 * - Literally "on top of that" - builds upon previous statement
 *
 * Kanji variants:
 * - その上 (standard)
 * - そのうえ (hiragana reading)
 *
 * GiNZA parse structure:
 * - その (PRON/DET) - demonstrative ("that")
 * - 上 (NOUN) - "top, above"
 * - Together as compoundADV or ADV
 * - dep=advmod or dep=discourse
 *
 * Different from similar conjunctions:
 * - それに (soreni) - "and besides" (neutral, casual)
 * - しかも (shikamo) - "moreover" (less formal, emphatic/surprising)
 * - おまけに (omake ni) - "on top of that" (often negative, colloquial)
 * - 更に (sara ni) - "further still" (progression/escalation)
 * - なお (nao) - "furthermore" (neutral, formal, simple addition)
 * - 上に (ue ni) - "in addition to" (prepositional, within sentence)
 */
export default linguisticRule('その上', (r) => {
  // その上 is a formal conjunction meaning "moreover, on top of that"
  // It typically appears at the beginning of a sentence to add information
  //
  // GiNZA typically parses その上 as:
  // - Compound noun phrase acting as adverbial conjunction
  // - lemma can be "その上" or sometimes split into その + 上
  // - pos: ADV (adverb) or compound ADV
  // - dep: advmod or discourse (sentence connector)
  //
  // The key is that it functions as a sentence-initial conjunction/adverb
  // that introduces additional information.
  //
  // Common forms:
  // - その上 (kanji form - standard)
  // - Can also be written in hiragana as そのうえ (but test data shows kanji)

  const sonoue = r.tok({
    text: 'その上',
    posOneOf: ['ADV', 'NOUN'],  // Can be ADV or compound NOUN acting as ADV
    lemma: 'その上',
  }, 'sonoue');

  r.capture(sonoue);
});
