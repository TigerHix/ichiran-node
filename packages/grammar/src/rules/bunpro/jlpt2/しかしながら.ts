import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: しかしながら (shikashinagara) - "however, nevertheless"
 *
 * A formal conjunction that connects two contrasting clauses. It's an emphatic
 * variant of しかし (however), formed by combining しかし + ながら (while/although).
 * More formal than しかし, が, だが, だけど, etc.
 *
 * Structures:
 * - [Statement A]. しかしながら、[Statement B].
 * - [Statement A]。しかしながら、[Statement B].
 * - しかしながら、[Statement B]. (at beginning of sentence)
 *
 * Examples:
 * - 日本は安全な国だと言われている。しかしながら１００％安全というわけでもない。
 *   (Japan is said to be a safe country. However, it is not 100% safe.)
 * - 精一杯頑張れば夢が叶うと言われている。しかしながら、人生はそう甘くない。
 *   (It is said that if you work as hard as you can, your dreams will come true. However, life is not so easy.)
 * - そのアイデアはいいと思います。しかしながら、我々の予算だとそのプランを実行することはできないでしょう。
 *   (I think the idea is good. However, our budget will not allow us to implement that plan.)
 *
 * Key characteristics:
 * - Formal/literary register
 * - Used at sentence boundaries
 * - Expresses contrast between two clauses
 * - More emphatic than しかし alone
 *
 * Kanji variants:
 * - しかしながら (standard hiragana - most common)
 * - 然しながら (kanji variant - rare)
 * - 併しながら (kanji variant - rare)
 *
 * Different from similar conjunctions:
 * - しかし (shikashi) - "however" (less formal)
 * - が (ga) - "but" (less formal, particle)
 * - だけど (dakedo) - "but" (casual)
 * - でも (demo) - "but" (casual)
 * - だが (daga) - "however" (informal/masculine)
 * - それなのに (soren noni) - "and yet, despite that" (conversational, emotional)
 * - しかし + ながら as separate tokens (different grammar)
 *
 * GiNZA parse structure:
 * - Often parsed as a single CONJ/SCONJ token
 * - Or parsed as compound: しかし (CONJ) + ながら (PART/SCONJ)
 */
export default linguisticRule('しかしながら', (r) => {
  // しかしながら is a fixed conjunction expression
  // It's an emphatic formal variant of しかし (however)

  r.either(
    // Pattern 1: Single token parsing (most common)
    // GiNZA often treats the entire expression as a single token
    (b1) => {
      const shikashinagara = b1.tok({
        textOneOf: ['しかしながら', '然しながら', '併しながら'],
        posOneOf: ['CONJ', 'SCONJ', 'ADV'],
      }, 'shikashinagara');

      b1.capture(shikashinagara);
    },

    // Pattern 2: Compound parsing - しかし + ながら
    // Some tokenizers may treat it as two tokens
    (b2) => {
      const shikashi = b2.tok({
        textOneOf: ['しかし', '然し', '併し'],
        posOneOf: ['CONJ', 'SCONJ', 'ADV'],
      }, 'shikashi');

      const nagara = b2.tok({
        text: 'ながら',
        posOneOf: ['PART', 'SCONJ', 'AUX'],
      }, 'nagara');

      b2.inOrder(shikashi, nagara, 3);

      // Capture the full expression
      b2.captureSpan('しかしながら', shikashi, nagara);
    },

    // Pattern 3: Minimal constraint pattern (catch-all)
    // Just match the sequence without specific POS/dep
    (b3) => {
      const shikashi = b3.tok({
        textOneOf: ['しかし', '然し', '併し'],
      }, 'shikashi');

      const nagara = b3.tok({
        text: 'ながら',
      }, 'nagara');

      b3.inOrder(shikashi, nagara, 3);

      b3.captureSpan('しかしながら', shikashi, nagara);
    }
  );
});
