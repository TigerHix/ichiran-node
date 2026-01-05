import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('としたら-とすれば-とすると', (r) => {
  // としたら・とすれば・とすると - "if we suppose/assuming that/in that case"
  r.either(
    // Pattern 1: と + し + たら (したら split form)
    (b) => {
      const to = b.tok({ text: 'と' }, 'to');
      const shi = b.tok({ text: 'し', lemma: 'する' }, 'shi');
      const tara = b.tok({ text: 'たら' }, 'tara');
      b.inOrder(to, shi, 1);
      b.inOrder(shi, tara, 1);
      b.captureSpan('としたら-とすれば-とすると', to, tara);
    },

    // Pattern 2: と + すれ + ば (すれば split form)
    (b) => {
      const to = b.tok({ text: 'と' }, 'to');
      const sure = b.tok({ text: 'すれ', lemma: 'する' }, 'sure');
      const ba = b.tok({ text: 'ば', pos: 'SCONJ' }, 'ba');
      b.inOrder(to, sure, 1);
      b.inOrder(sure, ba, 1);
      b.captureSpan('としたら-とすれば-とすると', to, ba);
    },

    // Pattern 3: と + する + と (すると split form)
    (b) => {
      const to1 = b.tok({ text: 'と' }, 'to1');
      const suru = b.tok({ text: 'する', lemma: 'する' }, 'suru');
      const to2 = b.tok({ text: 'と' }, 'to2');
      b.inOrder(to1, suru, 1);
      b.inOrder(suru, to2, 1);
      b.captureSpan('としたら-とすれば-とすると', to1, to2);
    }
  );
});
