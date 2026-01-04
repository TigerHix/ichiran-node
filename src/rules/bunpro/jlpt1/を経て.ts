import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('を経て', (r) => {
  // Match を + へる/経る + て pattern
  // GiNZA sometimes parses へ as へる with て as mark
  const wo = r.particle('を', 'wo');
  const heru = r.verb({ lemmaOneOf: ['へる', '経る'] }, 'heru');
  const te = r.tok({ text: 'て', pos: 'SCONJ' }, 'te');
  r.inOrder(wo, heru, 1).inOrder(heru, te, 1);
  r.captureSpan('を経て', wo, te);
});

