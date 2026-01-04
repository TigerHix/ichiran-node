import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('か何か', (r) => {
  // Pattern: Noun + か + 何か (or something, or something like that)
  // Examples: コーヒーか何か飲みたい, 風邪か何か引いたようだ
  //
  // GiNZA tokenization:
  // - "か何か" -> か (ADP) + 何 (PRON) + か (ADP)
  // - "かなにか" -> か (ADP) + なに (PRON) + か (ADP)

  // Use either to handle both "何" and "なに" forms
  r.either(
    // Branch 1: か + 何 + か
    (branch1) => {
      const ka1 = branch1.particle('か', 'ka1');
      const nani = branch1.tok({ pos: 'PRON', text: '何' }, 'nani');
      const ka2 = branch1.particle('か', 'ka2');
      branch1.inOrder(ka1, nani, 1).inOrder(nani, ka2, 1);
      branch1.captureSpan('か何か', ka1, ka2);
    },
    // Branch 2: か + なに + か
    (branch2) => {
      const ka1 = branch2.particle('か', 'ka1');
      const nani = branch2.tok({ pos: 'PRON', text: 'なに' }, 'nani');
      const ka2 = branch2.particle('か', 'ka2');
      branch2.inOrder(ka1, nani, 1).inOrder(nani, ka2, 1);
      branch2.captureSpan('か何か', ka1, ka2);
    }
  );
});
