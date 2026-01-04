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
    }
  );
});
