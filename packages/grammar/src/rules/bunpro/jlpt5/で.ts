import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('で', (r) => {
  // Locative/instrumental で particle (at/in/with/by means of)
  // NOT conjunction で (copula te-form)
  const de = r.particle('で', 'de', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'noun');
  r.caseMarker(noun, de);
  r.capture(de);
});
