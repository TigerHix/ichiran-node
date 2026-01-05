import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('は', (r) => {
  // Match topic marker は (wa)
  // Note: This will match は even in compound particles like には, では, etc.
  // From a linguistic perspective, は is still a topic marker in these cases.
  // More specific grammar rules (like JLPT3 では-それでは-じゃあ) handle
  // compound particles as distinct patterns.
  const wa = r.particle('は', 'wa', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM', 'PROPN', 'ADJ'] }, 'noun');
  r.caseMarker(noun, wa);
  r.capture(wa);
});
