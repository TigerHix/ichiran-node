import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('かどうか', (r) => {
  // Pattern: (verb/adj/noun) + か + どう + か (whether or not)

  // The first か (question particle)
  const ka1 = r.particle('か', 'ka1');

  // どう (adverb "how")
  const dou = r.tok({ text: 'どう', lemma: 'どう', pos: 'ADV' }, 'dou');

  // The second か (question particle)
  const ka2 = r.particle('か', 'ka2');

  // Require the sequence: か + どう + か (contiguous or close)
  r.inOrder(ka1, dou, 2).inOrder(dou, ka2, 2);

  // Capture the full pattern
  r.captureSpan('かどうか', ka1, ka2);
});
