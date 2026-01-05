import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: noun-型 (suffix meaning type/model/style)
 *
 * Noun + 型/形 = type, model, style, pattern, shape
 * Examples:
 * - 髪型 (hairstyle) - 髪 + 型
 * - 星形 (star shape) - 星 + 形
 * - ハート型 (heart-shaped) - ハート + 型
 * - 型のスマホ (type of smartphone) - standalone noun 型 + の + noun
 * - 新型のスマホ (new model) - i-adjective stem + 型
 *
 * Key patterns:
 * 1. Noun/Adj + 型/形 (suffix usage, rendaku)
 * 2. Noun/Adj + の + 型/形 (no rendaku after の)
 * 3. Test sentences use hiragana readings (がた/かた)
 *
 * GiNZA parse structure:
 * - 髪型: Single token (lemma=髪型) - GiNZA merges some compounds
 * - 星形: Single token (lemma=星形)
 * - ハート型の: ハート (NOUN) + 型 (NOUN, tag=接尾辞-名詞的-一般) + の (ADP)
 * - 型のスマホ: 型 (NOUN) + の (ADP)
 * - 新型の: 新しい (ADJ) + 型 (NOUN) + の (ADP)
 *
 * For test sentences with hiragana:
 * - 髪がた: 髪 (NOUN) + がた (NOUN, lemma=がた)
 * - 星がた: 星 (NOUN) + がた (NOUN, lemma=がた)
 * - 新しいかたの: 新しい (ADJ) + かた (NOUN) + の (ADP)
 * - 文けい: 文 (NOUN) + けい (PROPN)
 *
 * We match:
 * 1. Noun/Adj + 型/形/がた/かた/けい (suffix with lemma or text match)
 * 2. Noun/Adj + の + 型/形/かた (particle の in between)
 */
export default bunproLinguisticRule('noun-型', (r) => {
  r.either(
    // Pattern 1: Noun/Adj + 型/形 suffix (kanji or hiragana)
    (b) => {
      // Allow both NOUN and ADJ (e.g., 新しい + かた)
      const noun = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
      }, 'noun');

      // 型/形/がた/かた/けい as suffix
      // Match by text (surface) OR lemma
      // Also allow PROPN for cases like 文けい where けい is parsed as proper noun
      const gata = b.tok({
        textOneOf: ['型', '形', 'がた', 'かた', 'ケイ', 'けい'],
        lemmaOneOf: ['型', '形', 'がた', 'かた', 'けい'],
        posOneOf: ['NOUN', 'PROPN'],
      }, 'gata');

      b.inOrder(noun, gata, 1);
      b.captureSpan('noun-型', noun, gata);
    },

    // Pattern 2: Noun/Adj + の + 型/形/かた (no rendaku after の)
    (b) => {
      // Allow both NOUN and ADJ
      const noun = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
      }, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);

      // Note: after の, reading is always かた (not がた)
      const kata = b.tok({
        textOneOf: ['型', '形', 'かた'],
        lemmaOneOf: ['型', '形', 'かた'],
        pos: 'NOUN',
      }, 'kata');

      b.inOrder(no, kata, 1);
      b.captureSpan('noun-型', noun, kata);
    }
  );
});
