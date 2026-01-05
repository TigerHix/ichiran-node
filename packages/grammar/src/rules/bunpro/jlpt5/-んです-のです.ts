import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('-んです-のです', (r) => {
  const n = r.tok({ textOneOf: ['ん', 'の'] }, 'n');
  const desu = r.aux({ lemma: 'です' }, 'desu');
  r.inOrder(n, desu, 2);
  r.captureSpan('んです', n, desu);
});

