import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('たまでだ', (r) => {
  // Pattern: Verb[ta-form] + まで + (のこと)? + だ/です/で
  // Meaning: "merely did X", "just did X", emphasizes the action was nothing special
  // Examples: 言ったまでだ, 助けたまでだ, したまでのことだ

  // Match verb in ta-form with various inflection types
  // Also accepts AUX tokens as verb stems (for suru-verbs like した)
  r.either(
    // 連用形-一般 (e.g., した, した) - VERB or AUX
    (b) => {
      const verbStem = b.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般'
      }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const made = b.particle('まで', 'made');

      // Note: Not using auxOf here because in contracted forms like してた,
      // the ta auxiliary attaches to the main noun, not to the immediate verb stem
      b.inOrder(verbStem, ta, 3);
      b.inOrder(ta, made, 2);

      // Optional: のこと (nominalizer + noun)
      b.optional((ob) => {
        const no = ob.particle('の', 'no');
        const koto = ob.noun({ lemma: 'こと' }, 'koto');
        ob.inOrder(made, no, 1).inOrder(no, koto, 1);
      });

      // Copula: だ (casual), です (polite), or で (conjunctive)
      b.either(
        (cb) => {
          const da = cb.aux({ lemma: 'だ' }, 'da');
          cb.inOrder(made, da, 4);
          cb.captureSpan('たまでだ', verbStem, da);
        },
        (cb) => {
          const desu = cb.aux({ lemma: 'です' }, 'da');
          cb.inOrder(made, desu, 4);
          cb.captureSpan('たまでだ', verbStem, desu);
        },
        (cb) => {
          const de = cb.tok({ text: 'で', posOneOf: ['AUX', 'SCONJ', 'ADP'] }, 'da');
          cb.inOrder(made, de, 4);
          cb.captureSpan('たまでだ', verbStem, de);
        }
      );
    },
    // 連用形-イ音便 (e.g., いった)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-イ音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const made = b.particle('まで', 'made');

      b.inOrder(verbStem, ta, 2);
      b.inOrder(ta, made, 2);

      b.optional((ob) => {
        const no = ob.particle('の', 'no');
        const koto = ob.noun({ lemma: 'こと' }, 'koto');
        ob.inOrder(made, no, 1).inOrder(no, koto, 1);
      });

      b.either(
        (cb) => {
          const da = cb.aux({ lemma: 'だ' }, 'da');
          cb.inOrder(made, da, 4);
          cb.captureSpan('たまでだ', verbStem, da);
        },
        (cb) => {
          const desu = cb.aux({ lemma: 'です' }, 'da');
          cb.inOrder(made, desu, 4);
          cb.captureSpan('たまでだ', verbStem, desu);
        },
        (cb) => {
          const de = cb.tok({ text: 'で', posOneOf: ['AUX', 'SCONJ', 'ADP'] }, 'da');
          cb.inOrder(made, de, 4);
          cb.captureSpan('たまでだ', verbStem, de);
        }
      );
    },
    // 連用形-促音便 (e.g., 助けた)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-促音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const made = b.particle('まで', 'made');

      b.inOrder(verbStem, ta, 2);
      b.inOrder(ta, made, 2);

      b.optional((ob) => {
        const no = ob.particle('の', 'no');
        const koto = ob.noun({ lemma: 'こと' }, 'koto');
        ob.inOrder(made, no, 1).inOrder(no, koto, 1);
      });

      b.either(
        (cb) => {
          const da = cb.aux({ lemma: 'だ' }, 'da');
          cb.inOrder(made, da, 4);
          cb.captureSpan('たまでだ', verbStem, da);
        },
        (cb) => {
          const desu = cb.aux({ lemma: 'です' }, 'da');
          cb.inOrder(made, desu, 4);
          cb.captureSpan('たまでだ', verbStem, desu);
        },
        (cb) => {
          const de = cb.tok({ text: 'で', posOneOf: ['AUX', 'SCONJ', 'ADP'] }, 'da');
          cb.inOrder(made, de, 4);
          cb.captureSpan('たまでだ', verbStem, de);
        }
      );
    },
    // 連用形-撥音便 (e.g., した)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-撥音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const made = b.particle('まで', 'made');

      b.inOrder(verbStem, ta, 2);
      b.inOrder(ta, made, 2);

      b.optional((ob) => {
        const no = ob.particle('の', 'no');
        const koto = ob.noun({ lemma: 'こと' }, 'koto');
        ob.inOrder(made, no, 1).inOrder(no, koto, 1);
      });

      b.either(
        (cb) => {
          const da = cb.aux({ lemma: 'だ' }, 'da');
          cb.inOrder(made, da, 4);
          cb.captureSpan('たまでだ', verbStem, da);
        },
        (cb) => {
          const desu = cb.aux({ lemma: 'です' }, 'da');
          cb.inOrder(made, desu, 4);
          cb.captureSpan('たまでだ', verbStem, desu);
        },
        (cb) => {
          const de = cb.tok({ text: 'で', posOneOf: ['AUX', 'SCONJ', 'ADP'] }, 'da');
          cb.inOrder(made, de, 4);
          cb.captureSpan('たまでだ', verbStem, de);
        }
      );
    }
  );
});
