import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('を', (r) => {
  const wo = r.particle('を', 'wo', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'DET', 'NUM'] }, 'noun');
  r.caseMarker(noun, wo);
  r.capture(wo);
});
