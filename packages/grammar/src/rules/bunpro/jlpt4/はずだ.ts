import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: はずだ - Should be / Expected to be / Bound to be
 *
 * Matches clauses + はず + だ/です/だった to express expectation based on reasoning.
 *
 * Structures:
 * - Verb + はず + だ/です/だった
 * - ［い］Adjective + はず + だ/です/だった
 * - ［な］Adjective + な + はず + だ/です/だった
 * - Noun + の + はず + だ/です/だった
 *
 * Examples:
 * - 見つかるはずだ (should be found)
 * - 暑いはずだ (should be hot)
 * - 嫌いなはずだ (should dislike)
 * - 約束のはずだった (was supposed to be a promise)
 *
 * Key discriminators:
 * - はず is a NOUN (not a verb or auxiliary)
 * - Must be followed by copula (だ/です/だった)
 * - Attaches to various word types with different connectors (direct, な, or の)
 *
 * GiNZA parse structure:
 * - はず: NOUN with lemma=はず
 * - だ/です/だった: AUX with lemma=だ (copula)
 * - For verbs/adjectives: はず attaches directly with dep=cop or similar
 * - For na-adjectives: な auxiliary connects adjective to はず
 * - For nouns: の particle connects noun to はず
 *
 * Note: Standalone はず (without copula) at end of sentence is NOT matched
 * to avoid false positives on はずがない (different grammar point).
 */
export default bunproLinguisticRule('はずだ', (r) => {
  r.either(
    // Branch 1: はず + だ (casual, present)
    (b) => {
      const hazu = b.tok({ lemma: 'はず', pos: 'NOUN' }, 'hazu');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.inOrder(hazu, da, 1);
      b.captureSpan('はずだ', hazu, da);
    },

    // Branch 2: はず + です (polite, present)
    (b) => {
      const hazu = b.tok({ lemma: 'はず', pos: 'NOUN' }, 'hazu');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.inOrder(hazu, desu, 1);
      b.captureSpan('はずだ', hazu, desu);
    },

    // Branch 3: はず + だった (casual, past)
    (b) => {
      const hazu = b.tok({ lemma: 'はず', pos: 'NOUN' }, 'hazu');
      const datta = b.aux({ lemma: 'だった' }, 'datta');
      b.inOrder(hazu, datta, 1);
      b.captureSpan('はずだ', hazu, datta);
    },

    // Branch 4: な + はず + だ (na-adjective + casual, present)
    (b) => {
      const na = b.aux({ text: 'な', lemma: 'ない' }, 'na');
      const hazu = b.tok({ lemma: 'はず', pos: 'NOUN' }, 'hazu');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.inOrder(na, hazu, 1);
      b.inOrder(hazu, da, 1);
      b.captureSpan('はずだ', na, da);
    },

    // Branch 5: な + はず + です (na-adjective + polite, present)
    (b) => {
      const na = b.aux({ text: 'な', lemma: 'ない' }, 'na');
      const hazu = b.tok({ lemma: 'はず', pos: 'NOUN' }, 'hazu');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.inOrder(na, hazu, 1);
      b.inOrder(hazu, desu, 1);
      b.captureSpan('はずだ', na, desu);
    },

    // Branch 6: の + はず + だ (noun + casual, present)
    (b) => {
      const no = b.particle('の', 'no');
      const hazu = b.tok({ lemma: 'はず', pos: 'NOUN' }, 'hazu');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.inOrder(no, hazu, 1);
      b.inOrder(hazu, da, 1);
      b.captureSpan('はずだ', no, da);
    },

    // Branch 7: の + はず + です (noun + polite, present)
    (b) => {
      const no = b.particle('の', 'no');
      const hazu = b.tok({ lemma: 'はず', pos: 'NOUN' }, 'hazu');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.inOrder(no, hazu, 1);
      b.inOrder(hazu, desu, 1);
      b.captureSpan('はずだ', no, desu);
    },

    // Branch 8: の + はず + だった (noun + casual, past)
    (b) => {
      const no = b.particle('の', 'no');
      const hazu = b.tok({ lemma: 'はず', pos: 'NOUN' }, 'hazu');
      const datta = b.aux({ lemma: 'だった' }, 'datta');
      b.inOrder(no, hazu, 1);
      b.inOrder(hazu, datta, 1);
      b.captureSpan('はずだ', no, datta);
    }
  );
});
