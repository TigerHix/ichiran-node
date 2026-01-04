import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('が-but', (r) => {
  // Conjunction が (but/however) has pos=SCONJ, dep=mark
  // This is distinct from subject particle が which has pos=ADP, dep=case
  const ga = r.particle('が', 'ga', { pos: 'SCONJ', dep: 'mark' });
  r.capture(ga);
});
