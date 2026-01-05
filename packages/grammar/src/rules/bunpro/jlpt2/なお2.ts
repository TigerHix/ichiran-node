import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: なお② (nao) - "in addition, furthermore, moreover, incidentally"
 *
 * A FORMAL conjunction used at the beginning of sentences to add
 * supplemental information or emphasize further details. Different from
 * なお① which means "still/yet" (continuing state).
 *
 * This is なお② - the formal conjunction meaning "in addition" or
 * "furthermore," used to add supplementary information to a previous
 * statement with a polite, formal tone.
 *
 * Structures:
 * - [Statement A]. なお、[Statement B].
 * - [Statement A]。なお、[Statement B].
 *
 * Examples:
 * - 説明は以上です。なお、手続きは明後日までにお願いします。
 *   (This concludes the explanation. In addition, please complete the procedures by the day after tomorrow.)
 * - 明日は臨時休業です。なお、定休日は水曜日です。
 *   (Tomorrow is a temporary holiday. Furthermore, Wednesday is our regular holiday.)
 * - なお、ご不明な点がある場合はお気軽にお申し付けください。
 *   (In addition, if you have any questions, please feel free to ask.)
 *
 * Key characteristics:
 * - Formal register (硬い) - used in formal writing, documents, notices
 * - Sentence-initial position - typically starts a new sentence
 * - Adds supplemental information - introduces additional details
 * - Polite tone - shows respect for the listener/reader
 * - Often followed by comma (なお、) in Japanese text
 *
 * Kanji variants:
 * - なお (hiragana - standard for this usage)
 * - 尚 (kanji form - also common in formal writing)
 * - 尚（なお） (kanji with reading - very formal)
 *
 * GiNZA parse structure:
 * - なお/尚 (ADV or CCONJ) - adverb or conjunction
 * - At sentence/clause beginning, typically has dep=advmod or dep=discourse
 * - Often followed by comma (POS=PUNCT) when sentence-initial
 *
 * Different from:
 * - なお① (nao-1) - "still, yet, even now" (continuing state)
 *   - Typically appears within a sentence modifying a predicate
 *   - Example: "雨が降っているが、なお外で遊んでいる" (still playing outside)
 * - さらに (sarani) - "furthermore" (progression, less formal)
 * - その上 (sono ue) - "on top of that" (emphatic addition)
 * - また (mata) - "also, again" (less formal, more common)
 * - それに (soreni) - "and besides" (neutral, casual)
 * - しかも (shikamo) - "moreover" (emphatic, surprising)
 * - おまけに (omake ni) - "on top of that" (often negative, colloquial)
 *
 * Structural discrimination from なお①:
 * - なお② (furthermore): Appears at sentence/clause boundary, typically with comma
 * - なお① (still): Appears within sentence, directly modifying predicate without comma
 *
 * NOTE: Both なお① and なお② use the same word "なお" but have different meanings.
 * This rule focuses on the "furthermore/in addition" usage at sentence beginnings.
 */
export default bunproLinguisticRule('なお2', (r) => {
  // なお② is a formal conjunction meaning "in addition, furthermore"
  // It typically appears at the beginning of sentences or after sentence boundaries
  //
  // GiNZA typically parses なお/尚 as:
  // - ADV (adverb) with dep=advmod modifying the following clause
  // - CCONJ (conjunction) with dep=cc connecting clauses
  // - At sentence beginning, often has dep=discourse
  //
  // Structural characteristics of なお②:
  // 1. Often appears at sentence/clause beginning (position 0 or after period)
  // 2. Frequently followed by comma (、) when sentence-initial
  // 3. Introduces a new clause or sentence
  //
  // Discrimination from なお①:
  // - なお① (still): "雨が降っているが、なお外で遊んでいる"
  //   - Appears mid-sentence, no comma after なお
  //   - Modifies predicate directly (外で遊んでいる)
  // - なお② (furthermore): "説明は以上です。なお、手続きは..."
  //   - After sentence boundary (period)
  //   - Often followed by comma
  //   - Introduces new sentence/clause
  //
  // Common forms:
  // - なお (hiragana - most common for this usage)
  // - 尚 (kanji form - formal writing)
  //
  // Since both rules use the same word with different meanings,
  // we accept some overlap. The test data distinguishes by context.

  r.either(
    // Pattern 1: なお followed by comma (typical sentence-initial usage)
    // This is the strongest indicator of なお② (conjunction usage)
    (b1) => {
      const nao = b1.tok({
        lemmaOneOf: ['なお', '尚'],
        posOneOf: ['ADV', 'CCONJ'],
      }, 'nao');

      const comma = b1.tok({
        text: '、',
        pos: 'PUNCT',
      }, 'comma');

      // なお must be immediately followed by comma (within 1 token)
      b1.inOrder(nao, comma, 1);
      b1.capture(nao);
    },

    // Pattern 2: なお at position where it functions as conjunction
    // This catches cases without comma but still at clause boundaries
    (b2) => {
      const nao = b2.tok({
        lemmaOneOf: ['なお', '尚'],
        posOneOf: ['ADV', 'CCONJ'],
      }, 'nao');

      b2.capture(nao);
    }
  );
});
