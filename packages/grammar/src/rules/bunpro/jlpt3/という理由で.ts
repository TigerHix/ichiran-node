import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: という理由で - "for this reason", "on the grounds of", "with the reasoning that"
 *
 * Structure: (Reason) + という + 理由 + で
 * Variant: (Reason) + そういう + 理由 + で
 *
 * This pattern is used to give reasons about "the thing which is called (A)".
 * It puts emphasis on the reason itself, similar to "being that" in English.
 *
 * Examples:
 * - ストアのルールを破ったという理由で五つのアプリが削除されました。
 *   (5 apps were removed on the grounds of breaking the store rules.)
 * - 安くて美味しいという理由で、この店の前にはいつも行列ができています。
 *   (Being that this place is cheap and delicious, they always have a long line.)
 * - 大変だという理由で彼は仕事を辞めた。
 *   (Being that his job was a hassle, he resigned.)
 *
 * The case particle で shows "with" or "by means of" that reason.
 *
 * GiNZA parsing:
 * - と (ADP/particle) - quote particle
 * - いう (VERB) - quotative verb
 * - 理由/りゆう (NOUN) - "reason" (may be kanji or hiragana)
 * - で (ADP) - case particle (various dep: case, obl, etc.)
 *
 * Variants:
 * - という理由で (most common, with kanji or hiragana)
 * - そういう理由で (with "そういう" demonstrative, no と particle)
 *
 * Note: This rule does NOT match という理由では (topic form) or という理由だけで
 * (with だけ inserted before で).
 */
export default linguisticRule('という理由で', (r) => {
  r.either(
    // Pattern 1a: という理由で (with という, kanji)
    // 安くて美味しいという理由で、この店の前にはいつも行列ができています。
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const riyuu = b.noun({ text: '理由' }, 'riyuu');
      const de = b.particle('で', 'de');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, riyuu, 1);
      b.inOrder(riyuu, de, 1);

      b.captureSpan('という理由で', to, de);
    },

    // Pattern 1b: というりゆうで (with という, hiragana)
    // ストアのルールを破ったというりゆうで五つのアプリが削除されました。
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const riyuu = b.noun({ text: 'りゆう' }, 'riyuu');
      const de = b.particle('で', 'de');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, riyuu, 1);
      b.inOrder(riyuu, de, 1);

      b.captureSpan('という理由で', to, de);
    },

    // Pattern 2a: そういう理由で (with そういう, kanji)
    // 仕事がとても大変だった。そういう理由で、今日はゆっくりしたい。
    (b) => {
      const sou = b.tok({ text: 'そう', pos: 'ADV' }, 'sou');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const riyuu = b.noun({ text: '理由' }, 'riyuu');
      const de = b.particle('で', 'de');

      b.inOrder(sou, iu, 1);
      b.inOrder(iu, riyuu, 1);
      b.inOrder(riyuu, de, 1);

      b.captureSpan('という理由で', sou, de);
    },

    // Pattern 2b: そういうりゆうで (with そういう, hiragana)
    (b) => {
      const sou = b.tok({ text: 'そう', pos: 'ADV' }, 'sou');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const riyuu = b.noun({ text: 'りゆう' }, 'riyuu');
      const de = b.particle('で', 'de');

      b.inOrder(sou, iu, 1);
      b.inOrder(iu, riyuu, 1);
      b.inOrder(riyuu, de, 1);

      b.captureSpan('という理由で', sou, de);
    }
  );
});
