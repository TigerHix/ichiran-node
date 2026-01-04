import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('まえに', (r) => {
  // まえに (mae ni): before, in front of
  // Shows one action occurring prior to or in front of another
  //
  // Pattern 1: Verb [dictionary form] + まえに (e.g., 行くまえに, 食べるまえに)
  // Pattern 2: Noun + の + まえに (e.g., 食事のまえに, 寝るまえに)

  r.either(
    // Pattern 1: Verb + まえに
    (r1) => {
      const verb = r1.verb({}, 'verb');
      const mae = r1.tok({ textOneOf: ['前', 'まえ'] }, 'mae');
      const ni = r1.particle('に', 'ni');

      r1.inOrder(verb, mae, 3);
      r1.inOrder(mae, ni, 1);
      r1.headChild(verb, mae);

      r1.captureSpan('まえに', verb, ni);
    },
    // Pattern 2: Noun + の + まえに
    (r2) => {
      const noun = r2.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const no = r2.particle('の', 'no');
      const mae = r2.tok({ textOneOf: ['前', 'まえ'] }, 'mae');
      const ni = r2.particle('に', 'ni');

      r2.inOrder(noun, no, 1);
      r2.inOrder(no, mae, 1);
      r2.inOrder(mae, ni, 1);
      r2.headChild(noun, mae);

      r2.captureSpan('まえに', noun, ni);
    }
  );
});
