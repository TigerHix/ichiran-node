import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なにか-なにも', (r) => {
  // なにか (nanika) - "something, anything" - indefinite pronoun
  // なにも (nanimo) - "nothing, not anything" - used with negative verbs
  //
  // These are indefinite pronouns formed by combining the question word 何 (なに)
  // with particles か or も.
  //
  // なにか (something): Used in positive/interrogative contexts
  // - なにか食べますか？ (Do you want to eat something?)
  // - 彼はなにか答えを探している (He is searching for some kind of answer)
  //
  // なにも (nothing): Used with negative verbs (ない, ありません, etc.)
  // - なにも食べたくないです (I don't want to eat anything)
  // - 冷蔵庫の中にはなにもないです (There is nothing inside the refrigerator)
  //
  // Note: Casual forms include なんか, なんにか, なんにも, なんも

  r.either(
    // Branch 1: なにか (nanika) - "something, anything"
    (branch1) => {
      const nani = branch1.tok({ lemma: 'なに', pos: 'PRON' }, 'nani');
      const ka = branch1.particle('か', 'ka', { dep: 'case' });
      branch1.caseMarker(nani, ka);
      branch1.captureSpan('なにか', nani, ka);
    },
    // Branch 2: なにも (nanimo) - "nothing, not anything" (with negative verb)
    (branch2) => {
      const nani = branch2.tok({ lemma: 'なに', pos: 'PRON' }, 'nani');
      const mo = branch2.particle('も', 'mo', { dep: 'case' });
      branch2.caseMarker(nani, mo);
      branch2.captureSpan('なにも', nani, mo);
    }
  );
});
