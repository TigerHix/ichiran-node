import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('てから', (r) => {
  const te = r.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
  const kara = r.particle('から', 'kara', { pos: 'ADP', dep: 'case', lemma: 'から' });
  r.inOrder(te, kara, 1);
  r.captureSpan('てから', te, kara);
});
