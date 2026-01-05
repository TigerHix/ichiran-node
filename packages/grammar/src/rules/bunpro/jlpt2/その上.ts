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
  // GiNZA typically parses その上/そのうえ as:
  // - Often tokenized as two separate tokens: その + 上/うえ
  // - Sometimes as a single compound token
  // - Forms: その上 (kanji), そのうえ (hiragana)
  //
  // The key is that it functions as a sentence-initial conjunction/adverb
  // that introduces additional information.
  //
  // Common forms:
  // - その上 (kanji form - standard writing)
  // - そのうえ (hiragana form - commonly used in text)
  //
  // Since GiNZA may tokenize this as その + 上/うえ (two tokens),
  // we need to match both patterns.

  r.either(
    // Pattern 1: Single token その上/そのうえ
    (b1) => {
      const sonoue = b1.tok({
        textOneOf: ['その上', 'そのうえ'],
      }, 'sonoue');
      b1.capture(sonoue);
    },

    // Pattern 2: Two tokens: その + 上/うえ
    (b2) => {
      const sono = b2.tok({
        text: 'その',
        posOneOf: ['DET', 'PRON'],  // Demonstrative determiner/pronoun
      }, 'sono');
      const ue = b2.tok({
        textOneOf: ['上', 'うえ'],
        pos: 'NOUN',  // Noun meaning "top, above"
      }, 'ue');

      b2.inOrder(sono, ue, 1);  // Must be adjacent
      b2.captureSpan('その上', sono, ue);
    }
  );
});
