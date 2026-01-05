import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('もう', (r) => {
  // もう (mou) is an adverb meaning "already" or "anymore"
  // - With past tense: "already" (something happened unexpectedly soon)
  // - With negative: "anymore" (something stopped unexpectedly)
  // Opposite of まだ (mada - still/not yet)

  r.either(
    // Branch 1: もう as adverb modifying verbs/adj (dep='advmod')
    (branch) => {
      const mou = branch.adv({ lemma: 'もう', dep: 'advmod' }, 'mou');
      branch.capture(mou);
    },
    // Branch 2: もう as root adverb (standalone or at sentence start)
    (branch) => {
      const mou = branch.adv({ lemma: 'もう', dep: 'root' }, 'mou');
      branch.capture(mou);
    }
  );
});
