import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ところが', (r) => {
  // ところが (conjunction: "however", "unexpectedly")
  // Connects expectation with unexpected result
  // GiNZA tokenizes as either:
  // - Single token (text=ところが)
  // - Two tokens: ところ + が
  //
  // This is a conjunction (different from noun "place")
  // Used sentence-initially to introduce unexpected results
  r.either(
    // Pattern 1: Single token ところが
    (b) => {
      const tokoroga = b.tok({ text: 'ところが' }, 'tokoroga');
      b.capture(tokoroga);
    },
    // Pattern 2: Two tokens - ところ + が
    (b) => {
      const tokoro = b.tok({ text: 'ところ' }, 'tokoro');
      const ga = b.tok({ text: 'が' }, 'ga');
      b.inOrder(tokoro, ga, 1);
      b.captureSpan('ところが', tokoro, ga);
    }
  );
});
