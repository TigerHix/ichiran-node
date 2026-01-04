import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('すでに', (r) => {
  // すでに - formal adverb meaning "already"
  // A formal alternative to もう, with nuances:
  // - "already" (completion, can't go back)
  // - "too late" (nothing can be done)
  // - "beyond doubt" (absolutely certain)
  // - "nothing remaining" (completely gone)
  // - "all around" (entirely)
  //
  // POS='ADV' (adverb)
  // Used to modify verbs, adjectives, or appear at sentence start

<<<<<<< HEAD
=======
  const sudeni = r.adv({
    lemma: 'すでに',
  }, 'sudeni');

>>>>>>> jlpt3-sudeni
  r.either(
    // Branch 1: すでに as adverb modifying verbs/adj (dep='advmod')
    (branch) => {
      const sudeniAdv = branch.adv({
        lemma: 'すでに',
        dep: 'advmod',
      }, 'sudeni');
      branch.capture(sudeniAdv);
    },
    // Branch 2: すでに as root adverb (at sentence start)
    (branch) => {
      const sudeniRoot = branch.adv({
        lemma: 'すでに',
        dep: 'root',
      }, 'sudeni');
      branch.capture(sudeniRoot);
    }
  );
});
