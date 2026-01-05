import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('が', (r) => {
  const ga = r.particle('が', 'ga', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'noun');
  r.caseMarker(noun, ga);
  r.not((nr) => {
    const verb = nr.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
    nr.headChild(verb, ga);
  });
  r.capture(ga);
});
