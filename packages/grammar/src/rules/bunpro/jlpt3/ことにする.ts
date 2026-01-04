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
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.either(
        // 1a: Single する token (any POS, dep=fixed to distinguish from ことだ)
        (b2) => {
          const suru = b2.tok({ lemma: 'する', dep: 'fixed' }, 'suru');
          b2.inOrder(ni, suru);
          b2.captureSpan('ことにする', pred, suru);
        },
        // 1b: し + る decomposed (AUX + AUX)
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const ru = b2.aux({ lemma: 'る' }, 'ru');
          b2.auxOf(shi, ru);
          b2.captureSpan('ことにする', pred, ru);
        }
      );
    },

    // Branch 2: Verb + こと + に + した (past)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      // した is decomposed: し (AUX, dep=fixed) + た (AUX, dep=aux of pred)
      const shi = b.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
      b.inOrder(ni, shi, 1);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      const ta = b.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
      b.auxOf(pred, ta);

      b.captureSpan('ことにする', pred, ta);
    },

    // Branch 3: Verb + こと + に + します (polite present)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.either(
        // 3a: Single します token
        (b2) => {
          const shimasu = b2.aux({ lemma: 'ます', inflectionForm: '終止形-一般' }, 'shimasu');
          b2.inOrder(ni, shimasu, 3);
          b2.captureSpan('ことにする', pred, shimasu);
        },
        // 3b: し + ます decomposed
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const masu = b2.aux({ lemma: 'ます', inflectionForm: '終止形-一般' }, 'masu');
          b2.inOrder(shi, masu, 3);
          b2.captureSpan('ことにする', pred, masu);
        }
      );
    },

    // Branch 4: Verb + こと + に + しました (polite past)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.either(
        // 4a: Single しました token
        (b2) => {
          const shimashita = b2.aux({ lemma: 'ました' }, 'shimashita');
          b2.inOrder(koto, shimashita, 5);
          b2.captureSpan('ことにする', pred, shimashita);
        },
        // 4b: し + まし + た decomposed
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const mash = b2.aux({ lemma: 'ます', inflectionForm: '連用形-一般' }, 'mash');
          b2.inOrder(shi, mash, 3);
          const ta = b2.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
          b2.auxOf(pred, ta);
          b2.captureSpan('ことにする', pred, ta);
        }
      );
    },

    // Branch 5: Verb + ない + こと + に + する (negative verb, present)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      const suru = b.verb({ lemma: 'する', dep: 'fixed' }, 'suru');
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
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      // した is decomposed: し (AUX, dep=fixed) + た (AUX, dep=aux of pred)
      const shi = b.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
      b.inOrder(ni, shi, 1);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      const ta = b.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
      b.auxOf(pred, ta);

      b.captureSpan('ことにする', pred, ta);
    },

    // Branch 7: Verb + ない + こと + に + します (negative verb, polite present)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.either(
        // 7a: Single します token
        (b2) => {
          const shimasu = b2.aux({ lemma: 'ます', inflectionForm: '終止形-一般' }, 'shimasu');
          b2.inOrder(ni, shimasu, 3);
          b2.captureSpan('ことにする', pred, shimasu);
        },
        // 7b: し + ます decomposed
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const masu = b2.aux({ lemma: 'ます', inflectionForm: '終止形-一般' }, 'masu');
          b2.inOrder(shi, masu, 3);
          b2.captureSpan('ことにする', pred, masu);
        }
      );
    },

    // Branch 8: Verb + ない + こと + に + しました (negative verb, polite past)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.either(
        // 8a: Single しました token
        (b2) => {
          const shimashita = b2.aux({ lemma: 'ました' }, 'shimashita');
          b2.inOrder(koto, shimashita, 5);
          b2.captureSpan('ことにする', pred, shimashita);
        },
        // 8b: し + まし + た decomposed
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const mash = b2.aux({ lemma: 'ます', inflectionForm: '連用形-一般' }, 'mash');
          b2.inOrder(shi, mash, 3);
          const ta = b2.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
          b2.auxOf(pred, ta);
          b2.captureSpan('ことにする', pred, ta);
        }
      );
    },

    // Branch 9: Verb + こと + に + している (progressive/habitual)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.either(
        // 9a: Single している token
        (b2) => {
          const shiteiru = b2.aux({ lemma: 'ている' }, 'shiteiru');
          b2.inOrder(koto, shiteiru, 5);
          b2.captureSpan('ことにする', pred, shiteiru);
        },
        // 9b: し + て + いる decomposed
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const te = b2.aux({ lemma: 'て' }, 'te');
          b2.inOrder(shi, te, 1);
          const iru = b2.aux({ lemma: 'いる' }, 'iru');
          b2.inOrder(te, iru, 3);
          b2.captureSpan('ことにする', pred, iru);
        }
      );
    },

    // Branch 10: Verb + ない + こと + に + している (negative verb, progressive)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.either(
        // 10a: Single している token
        (b2) => {
          const shiteiru = b2.aux({ lemma: 'ている' }, 'shiteiru');
          b2.inOrder(koto, shiteiru, 5);
          b2.captureSpan('ことにする', pred, shiteiru);
        },
        // 10b: し + て + いる decomposed
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const te = b2.aux({ lemma: 'て' }, 'te');
          b2.inOrder(shi, te, 1);
          const iru = b2.aux({ lemma: 'いる' }, 'iru');
          b2.inOrder(te, iru, 3);
          b2.captureSpan('ことにする', pred, iru);
        }
      );
    },

    // Branch 11: Verb + こと + に + してる (casual progressive)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      const shi = b.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
      b.inOrder(ni, shi, 1);

      const te = b.aux({ lemma: 'て' }, 'te');
      b.inOrder(shi, te, 1);

      const ru = b.aux({ lemma: 'る' }, 'ru');
      b.inOrder(te, ru, 3);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, ru);
    },

    // Branch 12: Verb + ない + こと + に + してる (negative verb, casual progressive)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      const shi = b.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
      b.inOrder(ni, shi, 1);

      const te = b.aux({ lemma: 'て' }, 'te');
      b.inOrder(shi, te, 1);

      const ru = b.aux({ lemma: 'る' }, 'ru');
      b.inOrder(te, ru, 3);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, ru);
    },

    // Branch 13: Verb + こと + に + しない (negative of decision itself)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.either(
        // 13a: Single しない token
        (b2) => {
          const shinai = b2.verb({ text: 'しない', lemma: 'する' }, 'shinai');
          b2.inOrder(koto, shinai, 5);
          b2.captureSpan('ことにする', pred, shinai);
        },
        // 13b: し + ない decomposed
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const nai = b2.aux({ lemma: 'ない' }, 'nai');
          b2.inOrder(shi, nai, 1);
          b2.captureSpan('ことにする', pred, nai);
        }
      );
    },

    // Branch 14: Verb + ない + こと + に + しない (negative verb, negative decision)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');

      // First ない attaches to pred verb
      const predNai = b.aux({ lemma: 'ない' }, 'predNai');
      b.inOrder(predNai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, predNai);

      b.either(
        // 14a: Single しない token
        (b2) => {
          const shinai = b2.verb({ text: 'しない', lemma: 'する' }, 'shinai');
          b2.inOrder(koto, shinai, 5);
          b2.captureSpan('ことにする', pred, shinai);
        },
        // 14b: し + ない decomposed
        (b2) => {
          const shi = b2.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
          b2.inOrder(ni, shi, 1);
          const suruNai = b2.aux({ lemma: 'ない' }, 'suruNai');
          b2.inOrder(shi, suruNai, 1);
          b2.captureSpan('ことにする', pred, suruNai);
        }
      );
    },

    // Branch 15: Verb + こと + に + し (connective form)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      const shi = b.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
      b.inOrder(ni, shi, 1);

      const pred = b.verb({}, 'pred');
      b.inOrder(pred, koto);

      b.captureSpan('ことにする', pred, shi);
    },

    // Branch 16: Verb + ない + こと + に + し (negative verb, connective form)
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      const shi = b.aux({ lemma: 'する', dep: 'fixed', inflectionForm: '連用形-一般' }, 'shi');
      b.inOrder(ni, shi, 1);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.inOrder(nai, koto);

      const pred = b.verb({}, 'pred');
      b.auxOf(pred, nai);

      b.captureSpan('ことにする', pred, shi);
    }
  );
});
