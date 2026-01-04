import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことにする (koto ni suru) - decide to
 *
 * Matches verb + ことにする "decide to do"
 *
 * This expresses the speaker's volitional decision to take (or not take) an action.
 * It's a strong expression showing resolve in the decision.
 *
 * Structure:
 * - Verb［る］+ ことにする (casual present)
 * - Verb［ない］+ ことにする (casual present, negative)
 * - Verb［る］+ ことにした (casual past)
 * - Verb［ない］+ ことにした (casual past, negative)
 * - Verb［る］+ ことにします (polite present)
 * - Verb［ない］+ ことにします (polite present, negative)
 * - Verb［る］+ ことにしました (polite past)
 * - Verb［ない］+ ことにしました (polite past, negative)
 * - Verb［る］+ ことにしている (habitual decision)
 * - Verb［ない］+ ことにしている (habitual decision, negative)
 * - Verb［る］+ ことにしてる (casual progressive)
 * - Verb［ない］+ ことにしてる (casual progressive, negative)
 * - Verb［る］+ ことにしない (negative of the decision itself)
 * - Verb［ない］+ ことにしない (negative of the decision itself, double negative)
 * - Verb［る］+ ことにし (connective form)
 * - Verb［ない］+ ことにし (connective form, negative)
 *
 * Examples:
 * - 毎日、数分文法を勉強する事にした (I decided to study grammar every day)
 * - タマキさんと箱根に行くことにする (I decide to go to Hakone with Tamaki-san)
 * - 肉を食べないことにする (I decided not to eat meat)
 * - テレビゲームをしないことにします (I have decided not to play video games)
 *
 * GiNZA parse structure:
 * - 勉強する事にした: 勉強する(verb) + こと(noun) + に(particle) + した(verb+aux)
 * - 行くことにする: 行く(verb) + こと(noun) + に(particle) + する(verb)
 */
export default linguisticRule('ことにする', (r) => {
  r.either(
    // Branch 1: Verb［る］+ ことにする (casual present)
    (b) => {
      const pred = b.verb({}, 'pred'); // Any verb except する, なる, ある
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(pred, koto); // No maxDistance - find the right verb before こと

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const suru = b.verb({ lemma: 'する' }, 'suru');
      b.inOrder(ni, suru, 1);

      b.captureSpan('ことにする', pred, suru);
    },
    // Branch 2: Verb［ない］+ ことにする (casual present, negative)
    (b) => {
      const pred = b.verb({}, 'pred');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(pred, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const suru = b.verb({ lemma: 'する' }, 'suru');
      b.inOrder(ni, suru, 1);

      b.captureSpan('ことにする', pred, suru);
    },
    // Branch 3: Verb［る］+ ことにした (casual past)
    (b) => {
      const pred = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(pred, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.inOrder(ni, shita, 1);

      b.captureSpan('ことにする', pred, shita);
    },
    // Branch 4: Verb［ない］+ ことにした (casual past, negative)
    (b) => {
      const pred = b.verb({}, 'pred');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(pred, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.inOrder(ni, shita, 1);

      b.captureSpan('ことにする', pred, shita);
    },
    // Branch 5: Verb［る］+ ことにします (polite present)
    (b) => {
      const pred = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(pred, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shimasu = b.verb({ text: 'します', lemma: 'する' }, 'shimasu');
      b.inOrder(ni, shimasu, 1);

      b.captureSpan('ことにする', pred, shimasu);
    },
    // Branch 6: Verb［ない］+ ことにします (polite present, negative)
    (b) => {
      const pred = b.verb({}, 'pred');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(pred, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shimasu = b.verb({ text: 'します', lemma: 'する' }, 'shimasu');
      b.inOrder(ni, shimasu, 1);

      b.captureSpan('ことにする', pred, shimasu);
    },
    // Branch 7: Verb［る］+ ことにしました (polite past)
    (b) => {
      const pred = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(pred, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shimashita = b.verb({ text: 'しました', lemma: 'する' }, 'shimashita');
      b.inOrder(ni, shimashita, 1);

      b.captureSpan('ことにする', pred, shimashita);
    },
    // Branch 8: Verb［ない］+ ことにしました (polite past, negative)
    (b) => {
      const pred = b.verb({}, 'pred');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(pred, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shimashita = b.verb({ text: 'しました', lemma: 'する' }, 'shimashita');
      b.inOrder(ni, shimashita, 1);

      b.captureSpan('ことにする', pred, shimashita);
    },
    // Branch 9: Verb［る］+ ことにしている (habitual decision - progressive)
    (b) => {
      const pred = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(pred, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shiteiru = b.verb({ text: 'している', lemma: 'する' }, 'shiteiru');
      b.inOrder(ni, shiteiru, 1);

      b.captureSpan('ことにする', pred, shiteiru);
    },
    // Branch 10: Verb［ない］+ ことにしている (habitual decision - progressive, negative)
    (b) => {
      const pred = b.verb({}, 'pred');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(pred, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shiteiru = b.verb({ text: 'している', lemma: 'する' }, 'shiteiru');
      b.inOrder(ni, shiteiru, 1);

      b.captureSpan('ことにする', pred, shiteiru);
    },
    // Branch 11: Verb［る］+ ことにしてる (casual progressive)
    (b) => {
      const pred = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(pred, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shiteru = b.verb({ text: 'してる', lemma: 'する' }, 'shiteru');
      b.inOrder(ni, shiteru, 1);

      b.captureSpan('ことにする', pred, shiteru);
    },
    // Branch 12: Verb［ない］+ ことにしてる (casual progressive, negative)
    (b) => {
      const pred = b.verb({}, 'pred');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(pred, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shiteru = b.verb({ text: 'してる', lemma: 'する' }, 'shiteru');
      b.inOrder(ni, shiteru, 1);

      b.captureSpan('ことにする', pred, shiteru);
    },
    // Branch 13: Verb［る］+ ことにしない (negative of the decision itself)
    (b) => {
      const pred = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(pred, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shinai = b.verb({ text: 'しない', lemma: 'する' }, 'shinai');
      b.inOrder(ni, shinai, 1);

      b.captureSpan('ことにする', pred, shinai);
    },
    // Branch 14: Verb［ない］+ ことにしない (negative of the decision itself, double negative)
    (b) => {
      const pred = b.verb({}, 'pred');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(pred, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shinai = b.verb({ text: 'しない', lemma: 'する' }, 'shinai');
      b.inOrder(ni, shinai, 1);

      b.captureSpan('ことにする', pred, shinai);
    },
    // Branch 15: Verb［る］+ ことにし (connective form - te-form without て)
    (b) => {
      const pred = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(pred, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shi = b.verb({ text: 'し', lemma: 'する' }, 'shi');
      b.inOrder(ni, shi, 1);

      b.captureSpan('ことにする', pred, shi);
    },
    // Branch 16: Verb［ない］+ ことにし (connective form - te-form without て, negative)
    (b) => {
      const pred = b.verb({}, 'pred');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(pred, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);

      const shi = b.verb({ text: 'し', lemma: 'する' }, 'shi');
      b.inOrder(ni, shi, 1);

      b.captureSpan('ことにする', pred, shi);
    }
  );
});
