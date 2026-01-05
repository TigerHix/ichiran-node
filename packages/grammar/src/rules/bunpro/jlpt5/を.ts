import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('を', (r) => {
  const wo = r.particle('を', 'wo', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'DET', 'NUM'] }, 'noun');
  r.caseMarker(noun, wo);
  r.capture(wo);
});
