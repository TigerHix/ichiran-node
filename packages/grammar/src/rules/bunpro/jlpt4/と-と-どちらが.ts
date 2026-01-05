import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('と-と-どちらが', (r) => {
  // Pattern: A と B (と)、どちら(が/に/を) (between A and B, which...)
  // The second と is often omitted
  // The particle after どちら can be が, に, or を (or omitted with を選ぶ etc.)

  // First と particle (case marker 'and')
  const to1 = r.particle('と', 'to1');

  // どちら (which one - polite form)
  const dochira = r.tok({ textOneOf: ['どちら', 'どっち'] }, 'dochira');

  // Optional particle after どちら (が/に/を)
  const particle = r.tok({
    posOneOf: ['ADP', 'PART'],
    textOneOf: ['が', 'に', 'を']
  }, 'particle');

  // Require sequence: first と ... どちら ... optional particle
  r.inOrder(to1, dochira, 10);

  // Make the particle after どちら optional
  r.optional((b) => {
    b.inOrder(dochira, particle, 2);
  });

  // Capture from first と to end of pattern
  r.either(
    (b1) => {
      // With particle
      b1.captureSpan('と-と-どちらが', to1, particle);
    },
    (b2) => {
      // Without particle (just どちら)
      b2.captureSpan('と-と-どちらが', to1, dochira);
    }
  );
});
