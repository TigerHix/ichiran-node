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
    }
  );
});
