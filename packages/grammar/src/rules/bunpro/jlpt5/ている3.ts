import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ている3', (r) => {
  // ている③: Habitual/repeated actions
  // Verb[て] + いる (has/have done, does regularly)
  // Examples: 働いている (works/is working), 勉強している (studies/is studying)
  // This form expresses actions that happen regularly or habitually
  // (e.g., every day, every week), not just current progressive action
  //
  // Note: Structurally identical to いるている1 (progressive) and いるている2 (state change)
  // The distinction is semantic/contextual (frequency expressions like 毎日, 毎週)
  //
  // GiNZA parses verb-te + iru as:
  //   verb (conjunctive form, dep=advcl/root) + て/で (SCONJ, dep=mark) + いる (AUX/VERB)
  //
  // Key insight: いる (or its auxiliary い) has a dependency relationship
  // with the preceding te-form marker (て/で)

  r.either(
    // Branch 1: Standard form (verb-te + iru)
    (b1) => {
      const te = b1.tok(
        {
          textOneOf: ['て', 'で'],
          pos: 'SCONJ',
        },
        'te'
      );

      // いる can be either AUX or VERB depending on GiNZA's analysis
      // For dialectal contractions like "のんどる" (nde iru -> ndoru),
      // the lemma might be 'る' instead of 'いる'
      const iru = b1.tok(
        {
          lemmaOneOf: ['いる', 'る'],
          textOneOf: ['いる', 'る'], // ru for contracted form (e.g., 勉強してる)
          posOneOf: ['AUX', 'VERB'],
        },
        'iru'
      );

      // te comes right before iru
      b1.inOrder(te, iru, 1);

      // Capture from te to iru (includes contracted forms like してる)
      b1.captureSpan('ている3', te, iru);
    },

    // Branch 2: Polite form (verb-te + imasu)
    (b2) => {
      const te = b2.tok(
        {
          textOneOf: ['て', 'で'],
          pos: 'SCONJ',
        },
        'te'
      );

      const i = b2.tok(
        {
          lemma: 'いる',
          text: 'い',
        },
        'i'
      );

      const masu = b2.tok(
        {
          lemma: 'ます',
          textOneOf: ['ます', 'す'], // su for contracted form (e.g., 勉強してます)
        },
        'masu'
      );

      // Order: te + i + masu
      b2.inOrder(te, i, 1);
      b2.inOrder(i, masu, 1);

      // Capture from te to masu
      b2.captureSpan('ている3', te, masu);
    }
  );
});
