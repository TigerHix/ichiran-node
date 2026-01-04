import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('まえに', (r) => {
  // まえに (mae ni): before, in front of
  // Shows one action occurring prior to or in front of another
  //
  // GiNZA parsing:
  // - まえ: pos=NOUN
  // - に: pos=ADP, dep=case
  //
  // Pattern 1: Verb [dictionary form] + まえに (e.g., 行くまえに, 食べるまえに)
  //   Structure: verb (acl) → まえ (NOUN) → に (ADP/case)
  //   verb.head = まえ
  // Pattern 2: Noun + の + まえに (e.g., 食事のまえに)
  //   Structure: noun → の (ADP/case) → まえ (NOUN) → に (ADP/case)
  //   noun.head = まえ

  r.either(
    // Pattern 1: Verb + まえに
    (r1) => {
      const verb = r1.verb({}, 'verb');
      const mae = r1.noun({ textOneOf: ['前', 'まえ'] }, 'mae');
      const ni = r1.particle('に', 'ni');

      r1.inOrder(verb, mae, 5);  // Allow for auxiliaries (e.g., 勉強するまえに)
      r1.inOrder(mae, ni, 1);
      r1.headChild(mae, verb);  // verb's head is mae

      r1.captureSpan('まえに', verb, ni);
    },
    // Pattern 2: Noun/Pronoun + の + まえに
    (r2) => {
      const noun = r2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = r2.particle('の', 'no');
      const mae = r2.noun({ textOneOf: ['前', 'まえ'] }, 'mae');
      const ni = r2.particle('に', 'ni');

      r2.inOrder(noun, no, 1);
      r2.inOrder(no, mae, 1);
      r2.inOrder(mae, ni, 1);
      r2.headChild(mae, noun);  // noun's head is mae

      r2.captureSpan('まえに', noun, ni);
    },
    // Pattern 3: Determiner + まえに (e.g., そのまえに)
    (r3) => {
      const det = r3.tok({ pos: 'DET' }, 'det');
      const mae = r3.noun({ textOneOf: ['前', 'まえ'] }, 'mae');
      const ni = r3.particle('に', 'ni');

      r3.inOrder(det, mae, 1);
      r3.inOrder(mae, ni, 1);
      r3.headChild(mae, det);  // det's head is mae

      r3.captureSpan('まえに', det, ni);
    },
    // Pattern 4: まえに alone (at beginning of sentence)
    (r4) => {
      const mae = r4.noun({ textOneOf: ['前', 'まえ'] }, 'mae');
      const ni = r4.particle('に', 'ni');

      r4.inOrder(mae, ni, 1);

      r4.captureSpan('まえに', mae, ni);
    }
  );
});
