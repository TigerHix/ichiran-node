import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('かな', (r) => {
  // かな (I wonder) - sentence-final particle expressing uncertainty
  // GiNZA parses as two separate particles: か + な
  // Both have pos=PART and dep=mark, pointing to the sentence root
  const ka = r.tok({ text: 'か', dep: 'mark' }, 'ka');
  const na = r.tok({ text: 'な', dep: 'mark' }, 'na');

  // Must appear consecutively (immediate succession)
  r.inOrder(ka, na, 1);

  // Capture the full かな pattern
  r.captureSpan('かな', ka, na);
});
