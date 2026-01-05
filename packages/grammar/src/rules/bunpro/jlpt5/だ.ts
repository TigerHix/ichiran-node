import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('だ', (r) => {
  // Match the copula だ (auxiliary verb, lemma=だ)
  // Note: GiNZA uses dep='cop' for nouns, but dep='aux' for na-adjectives

  r.either(
    // Branch 1: Noun + copula だ (dep='cop')
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const da = branch.aux({ lemma: 'だ', dep: 'cop' }, 'da');
      branch.copulaOf(head, da);
      branch.capture(da);
    },
    // Branch 2: Na-adjective + だ (dep='aux')
    // Must NOT be an i-adjective
    // Discriminator: i-adjectives have conjugationClass='形容詞', na-adjectives have it undefined
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const da = branch.aux({ lemma: 'だ', dep: 'aux' }, 'da');
      branch.auxOf(naAdj, da);

      // The naAdj must not be an i-adjective
      // Use the same variable name so it checks the already-bound naAdj
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.capture(da);
    }
  );
});
