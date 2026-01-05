import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('や', (r) => {
  // や (ya): and, things like, and the like
  // Particle for non-exhaustive lists
  // Pattern: Noun + や + Noun

  const ya = r.particle('や', 'ya');

  r.capture(ya);
});
