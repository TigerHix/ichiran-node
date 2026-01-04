import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: さらに (sara ni) - "furthermore, moreover, even more"
 *
 * An adverb indicating that something continues or increases to a greater degree,
 * or that something additional happens. Can mean "furthermore," "moreover," "even more,"
 * "again," or "more and more."
 *
 * Structures:
 * - さらに + Phrase/Verb (modifies following predicate)
 * - 更に + Phrase/Verb (kanji form)
 *
 * Examples:
 * - 電気代がさらに高くなった。
 *   (The electricity bill became even more expensive.)
 * - 刺身には刺身専用の醤油をかけると、さらに美味しくなります。
 *   (Sashimi tastes even better when served with special soy sauce.)
 * - さらに行列が長くなり、お店の人も整理しきれなくなってしまった。
 *   (As the line grew even more, it became impossible for the staff to regulate everyone.)
 *
 * Key discriminators:
 * - Can be written as さらに (hiragana) or 更に (kanji)
 * - Always functions as an adverb (ADV)
 * - Modifies the following verb, adjective, or entire phrase
 * - Emphasizes progression, intensification, or accumulation
 *
 * GiNZA parse structure:
 * - さらに/更に (ADV) - adverb
 * - Followed by predicate (verb, adjective) or modifies entire clause
 *
 * Different from:
 * - また (mata) - "also, again" (less formal, broader meaning)
 * - その上 (sono ue) - "besides, in addition" (connective, emphasizes importance)
 * - しかも (shikamo) - "moreover, furthermore" (emphasizes surprising addition)
 * - ますます (masumasu) - "increasingly, more and more" (emphasizes ongoing process)
 */
export default linguisticRule('さらに', (r) => {
  // さらに/更に (furthermore, even more, again)
  // Adverb meaning "in addition to (A), (B)" or "even more than before"
  // Can be written in hiragana (さらに) or kanji (更に)

  const sarani = r.adv({
    lemmaOneOf: ['さらに', '更に'],
  }, 'sarani');

  // Capture just the adverb itself
  // It modifies whatever follows in the sentence
  r.capture(sarani);
});
