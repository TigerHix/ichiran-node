import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ごとに - Each, Every
 *
 * Matches patterns where ごとに (gotoni) is used to express "each" or "every".
 *
 * Structures:
 * - Noun + ごとに (each/every N)
 * - Verb + ごとに (every time [verb happens])
 *
 * Examples:
 * - ３時間ごとに水を飲んで下さ (Please drink water every 3 hours)
 * - 先生ごとにお土産をもらった (I received a souvenir from each teacher)
 * - 会う人ごとに笑顔で握手します (They greet every person they meet)
 * - 失敗をするごとに上達します (Every time you fail, you improve)
 *
 * Key discriminators:
 * - The particle ごとに is a suffix meaning "each/every"
 * - Can attach to nouns or verb phrases
 * - Focuses on "every individual instance"
 *
 * GiNZA parse structure:
 * - ごとに is usually parsed as a single ADP/PART token
 * - Sometimes NOUN+ごと is parsed as a compound with separate に
 */
export default bunproLinguisticRule('ごとに', (r) => {
  r.either(
    // Branch 1: ごとに as a single particle
    (b) => {
      const gotoni = b.particle('ごとに', 'gotoni');
      b.capture(gotoni);
    },
    // Branch 2: Token with text ending in ごと + に
    // This handles cases where NOUN+ごと is parsed as a compound
    (b) => {
      const goto = b.tok({ text: 'ごと' }, 'goto');
      const ni = b.particle('に', 'ni');
      b.inOrder(goto, ni, 1);
      b.captureSpan('ごとに', goto, ni);
    }
  );
});
