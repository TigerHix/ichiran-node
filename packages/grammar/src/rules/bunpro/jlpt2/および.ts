import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: および (oyobi) - "and, as well as"
 *
 * A formal conjunction connecting nouns in formal writing. Originally the
 * conjunctive form (ren'youkei) of the verb 及ぶ (oyobu - "to reach, extend to").
 * The literal meaning is "extending to (B)" when connecting (A) and (B).
 *
 * Structures:
 * - Noun + および + Noun
 * - Noun + 及び + Noun
 *
 * Examples:
 * - 免許証および印鑑を持ってきてください。
 *   (Please bring your driver's license and seal.)
 * - 夕食および朝食は別料金になります。
 *   (Dinner and breakfast are charged separately.)
 * - 顧客サポートおよび払戻に関する事項。
 *   (Matters related to refunds and customer support.)
 *
 * Key characteristics:
 * - Formal register (硬い) - used in written documents, business correspondence
 * - Connects nouns only (not clauses like そして or それから)
 * - Similar to と but much more formal
 * - Used in titles of documents, articles, and notifications
 * - Implies a complete list (unlike や which suggests partial list)
 *
 * Kanji/reading variants:
 * - および (hiragana - most common)
 * - 及び (kanji + hiragana - formal writing)
 *
 * GiNZA parse structure:
 * - Noun + および/及び (CCONJ)
 * - dep=cc (coordinating conjunction)
 *
 * Different from similar conjunctions:
 * - と (to) - "and" (casual, everyday conversation)
 * - や (ya) - "and things like" (partial list, less formal)
 * - そして (soshite) - "and then" (connects clauses/sentences, not just nouns)
 * - かつ (katsu) - "and" (formal, but emphasizes simultaneity)
 * - ならびに (narabini) - "and" (even more formal, often legal/academic)
 */
export default bunproLinguisticRule('および', (r) => {
  // および is a formal conjunction that connects two nouns
  // Pattern: Noun + および + Noun
  //
  // Forms:
  // - および (hiragana - most common)
  // - 及び (kanji form - formal writing)
  //
  // Key characteristics:
  // - Formal conjunction (CCONJ)
  // - Connects nouns in formal writing
  // - dep=cc (coordinating conjunction)
  // - Used in documents, business correspondence, official notices
  //
  // Similar but different from:
  // - と (casual "and" - everyday conversation)
  // - や (partial list - "and things like")
  // - そして (connects clauses, not just nouns)
  // - かつ (emphasizes simultaneity)
  // - ならびに (even more formal, legal/academic)

  const oyobi = r.tok({
    textOneOf: ['および', '及び'],
    pos: 'CCONJ',  // Coordinating conjunction
    dep: 'cc',     // Coordinating conjunction dependency
  }, 'oyobi');

  // Capture just the conjunction itself
  r.capture(oyobi);
});
