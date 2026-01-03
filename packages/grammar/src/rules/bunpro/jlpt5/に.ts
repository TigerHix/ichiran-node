import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('に', (r) => {
  // に as a dative particle (direction, time, location)
  const ni = r.particle('に', 'ni', { depOneOf: ['case', 'obl'] });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'noun');

  // Require に to mark a noun (case marking relationship)
  r.caseMarker(noun, ni);

  // Capture the に particle
  r.capture(ni);
});
