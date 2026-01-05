import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ずに済む (zu ni sumu) - "get by without doing"
 *
 * Verb negative stem + ずに + 済む = "get away without doing X"
 *
 * This pattern expresses that someone can avoid doing something unpleasant,
 * or that a situation was resolved without needing to do something undesirable.
 * It's the negative form of して済む (manage by doing X).
 *
 * Formation:
 * - Remove ない from the negative form and attach ずに + 済む
 * - Five-grade verbs (五段動詞): 待たない → 待たずに済む
 * - Ichidan verbs (一段動詞): 食べない → 食べずに済む
 * - Irregular verbs:
 *   - する → せずに済む (NOT しずに済む)
 *   - くる → こずに済む
 *
 * Conjugations of 済む:
 * - Present: ずに済む (casual), ずに済みます (polite)
 * - Past: ずに済んだ (casual), ずに済みました (polite)
 *
 * Examples:
 * - 今宿題をやっておけば後でやらずに済む (If I do homework now, I get by without doing it later)
 * - このVIPパスを使えば待たずに済みます (If you use this VIP pass, you get by without waiting)
 * - 知人の紹介で入社できたので、テストは受けずに済んだ (Since I joined through an acquaintance, got by without taking test)
 * - 事故が起きたけど、怪我をせずに済んだ (Accident occurred, but got by without getting injured)
 * - 宣伝費を使わずに済んだ (Got by without using advertising expenses)
 *
 * Note: Alternate forms include ないで済む and なくて済む (more common/modern)
 *
 * GiNZA parse patterns:
 * 1. ずに as AUX/SCONJ/PART + 済む(VERB) or 済んだ(VERB) or 渼みます(VERB+AUX)
 * 2. The ずに can have various POS tags (AUX, PART, SCONJ) and dep values
 * 3. 済む and its conjugations are the main verb
 *
 * Key discriminators:
 * - ずに must be the classical negative form (not ないで or なくて)
 * - Must be followed by some form of 済む (sumu - to finish/end)
 * - Different from just ずに (without doing) - this requires the completion verb
 */
export default linguisticRule('ずに済む', (r) => {
  r.either(
    // Pattern 1: ずに + 済む (present casual)
    // Example: やらずに済む (get by without doing)
    (b) => {
      const zuni = b.tok({
        textOneOf: ['ずに', 'ず'],
      }, 'zuni');
      const sumu = b.verb({
        lemma: 'すむ',
      }, 'sumu');
      b.inOrder(zuni, sumu, 3);
      b.captureSpan('ずに済む', zuni, sumu);
    },

    // Pattern 2: ずに + 渼んだ (past casual)
    // Example: やらずに済んだ (got by without doing)
    (b) => {
      const zuni = b.tok({
        textOneOf: ['ずに', 'ず'],
      }, 'zuni');
      const sumu = b.verb({
        lemma: 'すむ',
        // Past forms: 済んだ, 済みました
      }, 'sumu');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');
      b.inOrder(zuni, sumu, 3);
      b.inOrder(sumu, ta, 3);
      b.captureSpan('ずに済む', zuni, ta);
    },

    // Pattern 3: ずに + 渼みます (present polite)
    // Example: 待たずに済みます (get by without waiting - polite)
    (b) => {
      const zuni = b.tok({
        textOneOf: ['ずに', 'ず'],
      }, 'zuni');
      const sumi = b.verb({
        lemma: 'すむ',
        inflectionForm: '連用形-一般',
      }, 'sumi');
      const masu = b.aux({
        lemma: 'ます',
      }, 'masu');
      b.inOrder(zuni, sumi, 3);
      b.inOrder(sumi, masu, 1);
      b.captureSpan('ずに済む', zuni, masu);
    },

    // Pattern 4: ずに + 渼みました (past polite)
    // Example: 失敗せずに済みました (got by without failing - polite)
    (b) => {
      const zuni = b.tok({
        textOneOf: ['ずに', 'ず'],
      }, 'zuni');
      const sumi = b.verb({
        lemma: 'すむ',
        inflectionForm: '連用形-一般',
      }, 'sumi');
      const mashita = b.aux({
        lemma: 'ます',
        inflectionForm: '連用形-一般',
      }, 'mashita');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');
      b.inOrder(zuni, sumi, 3);
      b.inOrder(sumi, mashita, 1);
      b.inOrder(mashita, ta, 3);
      b.captureSpan('ずに済む', zuni, ta);
    }
  );
});
