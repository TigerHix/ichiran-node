import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('じゃない', (r) => {
  // Match the casual negative copula じゃない (is not)
  // Pattern: noun/na-adj + じゃ + ない
  // じゃ is a contraction of では
  // Note: GiNZA uses dep='cop' for nouns, but dep='aux' for na-adjectives

  r.either(
    // Branch 1: Noun + じゃ + ない (dep='cop')
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'cop' }, 'ja');
      const nai = branch.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');

      branch.copulaOf(head, ja);
      branch.headChild(ja, nai, 'fixed');
      branch.captureSpan('じゃない', ja, nai);
    },
    // Branch 2: Na-adjective + じゃ + ない (dep='aux')
    // Must NOT be an i-adjective
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'aux' }, 'ja');
      const nai = branch.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');

      branch.auxOf(naAdj, ja);
      branch.headChild(ja, nai, 'fixed');

      // The naAdj must not be an i-adjective
      // Use the same variable name so it checks the already-bound naAdj
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('じゃない', ja, nai);
    }
  );
});
