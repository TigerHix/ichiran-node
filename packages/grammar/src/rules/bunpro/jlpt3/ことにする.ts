import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことにする (koto ni suru) - decide to
 *
 * Matches verb + ことにする "decide to do"
 *
 * This expresses the speaker's volitional decision to take (or not take) an action.
 *
 * Structure:
 * - Verb［る］+ ことにする (casual present)
 * - Verb［ない］+ ことにする (casual present, negative)
 * - Verb［る］+ ことにした (casual past)
<<<<<<< HEAD
 * - Verb［る］+ ことにしています (habitual)
 * - Verb［る］+ ことにしない (negative decision)
 */
export default linguisticRule('ことにする', (r) => {
  r.either(
    // Branch 1: Match ことにする (present form)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.inOrder(koto, ni);
      b.inOrder(ni, suru, 1);
      b.captureSpan('ことにする', koto, suru);
    },

    // Branch 1b: Match ことにする (any text match for suru - more permissive)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const suru = b.tok({ text: 'する' }, 'suru');
      b.inOrder(koto, ni);
      b.inOrder(ni, suru, 1);
      b.captureSpan('ことにする', koto, suru);
    },

    // Branch 1c: Match ことにする (suru as any token with text=する, any POS)
    (b) => {
      const koto = b.tok({ textOneOf: ['こと', '事'] }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const suru = b.verb({ text: 'する' }, 'suru');
      b.inOrder(koto, ni);
      b.inOrder(ni, suru, 1);
      b.captureSpan('ことにする', koto, suru);
    },

    // Branch 1d: Match ことにする (without explicit ni, just koto followed by suru)
    (b) => {
      const koto = b.tok({ textOneOf: ['こと', '事'] }, 'koto');
      const suru = b.tok({ text: 'する' }, 'suru');
      b.inOrder(koto, suru, 2); // koto + ni + suru
      b.captureSpan('ことにする', koto, suru);
    },

    // Branch 2: Match ことにした (past)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.inOrder(koto, ni);
      b.inOrder(ni, shita, 1);
      b.captureSpan('ことにする', koto, shita);
    },

    // Branch 3: Match ことにします (polite present)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const shimasu = b.verb({ text: 'します', lemma: 'する' }, 'shimasu');
      b.inOrder(koto, ni);
      b.inOrder(ni, shimasu, 1);
      b.captureSpan('ことにする', koto, shimasu);
    },

    // Branch 4: Match ことにしました (polite past)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const shimashita = b.verb({ text: 'しました', lemma: 'する' }, 'shimashita');
      b.inOrder(koto, ni);
      b.inOrder(ni, shimashita, 1);
      b.captureSpan('ことにする', koto, shimashita);
    },

    // Branch 5: Match ことにしている (habitual)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const shite = b.tok({ text: 'して' }, 'shite');
      const iru = b.aux({ text: 'いる' }, 'iru');
      b.inOrder(koto, ni);
      b.inOrder(ni, shite, 1);
      b.inOrder(shite, iru, 1);
      b.captureSpan('ことにする', koto, iru);
    },

    // Branch 6: Match ことにしてる (casual progressive)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const shiteru = b.tok({ text: 'してる' }, 'shiteru');
      b.inOrder(koto, ni);
      b.inOrder(ni, shiteru, 1);
      b.captureSpan('ことにする', koto, shiteru);
    },

    // Branch 7: Match ことにしない (negative decision)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const shinai = b.tok({ text: 'しない' }, 'shinai');
      b.inOrder(koto, ni);
      b.inOrder(ni, shinai, 1);
      b.captureSpan('ことにする', koto, shinai);
    },

    // Branch 8: Match ことにし (connective form)
    (b) => {
      const koto = b.tok({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ text: 'に' }, 'ni');
      const shi = b.tok({ text: 'し' }, 'shi');
      b.inOrder(koto, ni);
      b.inOrder(ni, shi, 1);
      b.captureSpan('ことにする', koto, shi);
=======
 * - Verb［ない］+ ことにした (casual past, negative)
 * - Verb［る］+ ことにします (polite present)
 * - Verb［ない］+ ことにします (polite present, negative)
 * - Verb［る］+ ことにしました (polite past)
 * - Verb［ない］+ ことにしました (polite past, negative)
 * - Verb［る］+ ことにしている (habitual)
 * - Verb［ない］+ ことにしている (habitual, negative)
 * - Verb［る］+ ことにしてる (casual progressive)
 * - Verb［ない］+ ことにしてる (casual progressive, negative)
 * - Verb［る］+ ことにしない (negative decision)
 * - Verb［ない］+ ことにしない (negative decision, double negative)
 * - Verb［る］+ ことにし (connective form)
 * - Verb［ない］+ ことにし (connective form, negative)
 */
export default linguisticRule('ことにする', (r) => {
  r.either(
    // Branch 1: Verb + こと + に + する (dictionary form present)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.inOrder(koto, ni, suru);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, suru);
    },

    // Branch 2: Verb + こと + に + した (past)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.inOrder(koto, ni, shita);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, shita);
    },

    // Branch 3: Verb + こと + に + します (polite present)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shimasu = b.verb({ text: 'します', lemma: 'する' }, 'shimasu');
      b.inOrder(koto, ni, shimasu);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, shimasu);
    },

    // Branch 4: Verb + こと + に + しました (polite past)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shimashita = b.verb({ text: 'しました', lemma: 'する' }, 'shimashita');
      b.inOrder(koto, ni, shimashita);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, shimashita);
    },

    // Branch 5: Verb + ない + こと + に + する (negative verb, present)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.inOrder(koto, ni, suru);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, suru);
    },

    // Branch 6: Verb + ない + こと + に + した (negative verb, past)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.inOrder(koto, ni, shita);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, shita);
    },

    // Branch 7: Verb + ない + こと + に + します (negative verb, polite present)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shimasu = b.verb({ text: 'します', lemma: 'する' }, 'shimasu');
      b.inOrder(koto, ni, shimasu);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, shimasu);
    },

    // Branch 8: Verb + ない + こと + に + しました (negative verb, polite past)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shimashita = b.verb({ text: 'しました', lemma: 'する' }, 'shimashita');
      b.inOrder(koto, ni, shimashita);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, shimashita);
    },

    // Branch 9: Verb + こと + に + している (progressive/habitual)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shiteiru = b.verb({ text: 'している', lemma: 'する' }, 'shiteiru');
      b.inOrder(koto, ni, shiteiru);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, shiteiru);
    },

    // Branch 10: Verb + ない + こと + に + している (negative verb, progressive)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shiteiru = b.verb({ text: 'している', lemma: 'する' }, 'shiteiru');
      b.inOrder(koto, ni, shiteiru);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, shiteiru);
    },

    // Branch 11: Verb + こと + に + してる (casual progressive)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shiteru = b.verb({ text: 'してる', lemma: 'する' }, 'shiteru');
      b.inOrder(koto, ni, shiteru);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, shiteru);
    },

    // Branch 12: Verb + ない + こと + に + してる (negative verb, casual progressive)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shiteru = b.verb({ text: 'してる', lemma: 'する' }, 'shiteru');
      b.inOrder(koto, ni, shiteru);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, shiteru);
    },

    // Branch 13: Verb + こと + に + しない (negative of decision itself)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shinai = b.verb({ text: 'しない', lemma: 'する' }, 'shinai');
      b.inOrder(koto, ni, shinai);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, shinai);
    },

    // Branch 14: Verb + ない + こと + に + しない (negative verb, negative decision)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shinai = b.verb({ text: 'しない', lemma: 'する' }, 'shinai');
      b.inOrder(koto, ni, shinai);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, shinai);
    },

    // Branch 15: Verb + こと + に + し (connective form)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shi = b.verb({ text: 'し', lemma: 'する' }, 'shi');
      b.inOrder(koto, ni, shi);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, shi);
    },

    // Branch 16: Verb + ない + こと + に + し (negative verb, connective form)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.particle({ text: 'に' }, 'ni');
      const shi = b.verb({ text: 'し', lemma: 'する' }, 'shi');
      b.inOrder(koto, ni, shi);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, shi);
>>>>>>> jlpt3-koto-ni-suru
    }
  );
});
