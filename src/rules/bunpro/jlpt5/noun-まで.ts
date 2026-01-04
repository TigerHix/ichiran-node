import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('noun-まで', (r) => {
  // まで particle indicating "until" or "up to" a limit in time/space
  const made = r.particle('まで', 'made', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET', 'NUM', 'ADV'] }, 'noun');
  r.caseMarker(noun, made);
  r.capture(made);
});
