import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('としたら-とすれば-とすると', (r) => {
  // としたら・とすれば・とすると - "if we suppose/assuming that/in that case"
  r.either(
    // Pattern 1: と + し + た + ら (したら split form)
    (b) => {
      const to = b.tok({ text: 'と' }, 'to');
      const shi = b.tok({ text: 'し', lemma: 'する' }, 'shi');
      const ta = b.tok({ text: 'た' }, 'ta');
      const ra = b.tok({ text: 'ら' }, 'ra');
      b.inOrder(to, shi, 1);
      b.inOrder(shi, ta, 1);
      b.inOrder(ta, ra, 1);
      const head = b.tok({}, 'head');
      b.headChild(ra, head, 'obl');
      b.captureSpan('としたら-とすれば-とすると', head, ra);
    },

    // Pattern 2: と + すれ + ば (すれば split form)
    (b) => {
      const to = b.tok({ text: 'と' }, 'to');
      const sure = b.tok({ text: 'すれ', lemma: 'する' }, 'sure');
      const ba = b.tok({ text: 'ば', pos: 'SCONJ' }, 'ba');
      b.inOrder(to, sure, 1);
      b.inOrder(sure, ba, 1);
      const head = b.tok({}, 'head');
      b.headChild(ba, head, 'obl');
      b.captureSpan('としたら-とすれば-とすると', head, ba);
    },

    // Pattern 3: と + し + と (すると split form)
    (b) => {
      const to1 = b.tok({ text: 'と' }, 'to1');
      const shi = b.tok({ text: 'し', lemma: 'する' }, 'shi');
      const to2 = b.tok({ text: 'と' }, 'to2');
      b.inOrder(to1, shi, 1);
      b.inOrder(shi, to2, 1);
      const head = b.tok({}, 'head');
      b.headChild(to2, head, 'obl');
      b.captureSpan('としたら-とすれば-とすると', head, to2);
    }
  );
});
