import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ないわけにはいかない (nai wake ni wa ikanai) - Can't not, No way to avoid
 *
 * A strong expression highlighting that not doing (A) is impossible or unacceptable.
 * This is a double negative construction meaning "must do (A)" or "can't avoid doing (A)".
 *
 * Structures:
 * - Verb［negative］+ わけには + いかない (casual)
 * - Verb［negative］+ わけには + いきません (polite)
 * - Alternate: Verb［negative］+ わけには + いかないです (polite)
 *
 * Examples:
 * - 行かないわけにはいかない。
 *   (I can't not go / I have no choice but to go.)
 * - 宿題をしないわけにはいきません。
 *   (I must do my homework / I can't avoid doing my homework.)
 * - 面倒だからといって、打ち合わせをしないわけにはいかない。
 *   (Even though it is troublesome, there is no way to avoid having a meeting.)
 *
 * Key discriminators:
 * - Double negative: verb(nai) + wake(ni wa) + iku(nai)
 * - Expresses strong necessity or unavoidable obligation
 * - Different from simple ないといけない/なければならない (weaker obligation)
 * - Different from わけにはいかない (JLPT3 - opposite meaning: "can't do")
 * - Different from ざるを得ない (more formal, emphasizes compulsion)
 * - Often used with からといって (just because... doesn't mean...)
 * - Similar to ないわけにはいかない (literary variant)
 *
 * GiNZA parse structure:
 * - Verb(lemma) + ない(AUX, lemma=ない) + わけ(NOUN) + に(ADP) + は(PART/ADP) + 行く(VERB) + ない(AUX)
 * - Polite: Verb + ない + わけ + に + は + 行き(VERB, ren'youkei) + ません(AUX)
 *
 * Important: Matches the complete pattern verb-negative + わけには + いかない/いきません
 * to exclude:
 * - Simple verb-negative without わけにはいかない
 * - わけにはいかない (JLPT3 - opposite meaning, no verb-negative before it)
 * - ざるを得ない (more formal literary form)
 * - ないわけがない (verb-negative + わけがない - "no way that...")
 */
export default bunproLinguisticRule('ないわけにはいかない', (r) => {
  r.either(
    // Branch 1: Casual form - verb(negative) + わけには + いかない
    // Structure: ...ない + わけ + に + は + いか/い + ない
    (b1) => {
      // First, match the fixed phrase part: には + いか/い + ない
      const ni = b1.particle('に', 'ni');
      const wa = b1.particle('は', 'wa');
      const ika = b1.tok({
        textOneOf: ['い', 'いか', 'お', 'おい'],
      }, 'ika');

      // Match the second ない (after ika)
      const nai2 = b1.aux({
        lemma: 'ない',
      }, 'nai2');

      // Match わけ - try multiple matching strategies
      const wake = b1.tok({
        textOneOf: ['わけ', 'ワケ', 'ワケ', '訳'],
      }, 'wake');

      // Now look backwards for the first ない that comes before wake
      const nai = b1.aux({
        lemma: 'ない',
      }, 'nai');

      // Require sequential order
      b1.inOrder(nai, wake, 3);
      b1.inOrder(wake, ni, 1);
      b1.inOrder(ni, wa, 1);
      b1.inOrder(wa, ika, 5);
      b1.inOrder(ika, nai2, 5);
      // Ensure nai and nai2 are different
      b1.inOrder(nai, nai2, 20);

      // Capture from first ない to final ない
      b1.captureSpan('ないわけにはいかない', nai, nai2);
    },

    // Branch 1b: Alternative pattern where wake might be parsed differently
    // This handles cases where GiNZA might tokenize the pattern differently
    (b1b) => {
      // Match the fixed phrase first: には + いか + ない
      const ni = b1b.particle('に', 'ni');
      const wa = b1b.particle('は', 'wa');
      const ika = b1b.tok({
        textOneOf: ['い', 'いか', 'お', 'おい'],
      }, 'ika');

      const nai2 = b1b.aux({
        lemma: 'ない',
      }, 'nai2');

      // Now match the first nai that comes before the pattern
      const nai = b1b.aux({
        lemma: 'ない',
      }, 'nai');

      // Ensure nai comes before the pattern and nai2 comes after
      b1b.inOrder(nai, ni, 10);
      b1b.inOrder(ni, wa, 1);
      b1b.inOrder(wa, ika, 5);
      b1b.inOrder(ika, nai2, 5);
      // Ensure nai and nai2 are different tokens by requiring nai before nai2
      b1b.inOrder(nai, nai2, 20);

      b1b.captureSpan('ないわけにはいかない', nai, nai2);
    },

    // Branch 2: Polite form - verb(negative) + わけには + いきません
    // Structure: ...ない + わけ + に + は + 行き(ren'youkei) + ません
    (b2) => {
      // Negative auxiliary (ない)
      const nai = b2.aux({
        lemma: 'ない',
      }, 'nai');

      // わけ (noun)
      const wake = b2.tok({
        text: 'わけ',
      }, 'wake');

      // Particle に
      const ni = b2.particle('に', 'ni');

      // Particle は
      const wa = b2.particle('は', 'wa');

      // The "iki" part - ren'youkei form of 行く
      const iki = b2.tok({
        textOneOf: ['いき', 'おき'],
      }, 'iki');

      // Polite negative - matches "ません" or "せん" (archaic)
      const masen = b2.tok({
        textOneOf: ['ませ', 'せ'],
      }, 'masen');

      // Final ん (from ません)
      const n = b2.aux({
        lemma: 'ぬ',
        text: 'ん',
      }, 'n');

      // Require sequential order: ない + わけ + に + は + いき + まse + ん
      b2.inOrder(nai, wake, 5);
      b2.inOrder(wake, ni, 1);
      b2.inOrder(ni, wa, 1);
      b2.inOrder(wa, iki, 5);
      b2.inOrder(iki, masen, 3);
      b2.inOrder(masen, n, 3);

      // Capture from first ない to final ん
      b2.captureSpan('ないわけにはいかない', nai, n);
    },

    // Branch 3: Polite form with です - verb(negative) + わけには + いかないです
    // Structure: ...ない + わけ + に + は + いか/い + ない + です
    (b3) => {
      // Negative auxiliary (ない)
      const nai = b3.aux({
        lemma: 'ない',
      }, 'nai');

      // わけ (noun)
      const wake = b3.tok({
        text: 'わけ',
      }, 'wake');

      // Particle に
      const ni = b3.particle('に', 'ni');

      // Particle は
      const wa = b3.particle('は', 'wa');

      // The "ika" part
      const ika = b3.tok({
        textOneOf: ['い', 'いか', 'お', 'おい'],
      }, 'ika');

      // Negative auxiliary (ない)
      const nai2 = b3.aux({
        lemma: 'ない',
      }, 'nai2');

      // Copula (です/だ)
      const desu = b3.aux({
        lemmaOneOf: ['だ', 'です'],
      }, 'desu');

      // Require sequential order: ない + わけ + に + は + [い/いか] + ない + です
      b3.inOrder(nai, wake, 5);
      b3.inOrder(wake, ni, 1);
      b3.inOrder(ni, wa, 1);
      b3.inOrder(wa, ika, 5);
      b3.inOrder(ika, nai2, 5);
      b3.inOrder(nai2, desu, 3);

      // Capture from first ない to です
      b3.captureSpan('ないわけにはいかない', nai, desu);
    }
  );
});
