import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('へ', (r) => {
  const he = r.particle('へ', 'he', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET', 'NUM'] }, 'noun');
  r.caseMarker(noun, he);
  r.capture(he);
});
