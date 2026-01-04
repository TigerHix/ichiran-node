import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('-てこそ', (r) => {
  const te = r.tok({ text: 'て' }, 'te');
  const koso = r.tok({ text: 'こそ' }, 'koso');
  r.inOrder(te, koso, 1);
  r.captureSpan('てこそ', te, koso);
});

