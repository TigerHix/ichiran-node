import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('から', (r) => {
  const kara = r.particle('から', 'kara', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET', 'NUM', 'ADV'] }, 'noun');
  r.caseMarker(noun, kara);
  r.capture(kara);
});
