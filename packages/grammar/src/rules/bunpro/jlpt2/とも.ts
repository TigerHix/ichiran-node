import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('とも', (r) => {
  r.either(
    // Pattern 1: Verb volitional + とも (e.g., 言おうとも, 暴れようとも, なろうとも)
    (b1) => {
      const volitional = b1.tok({ inflectionForm: '意志推量形', posOneOf: ['AUX', 'VERB'] }, 'volitional');
      const tomo = b1.particle('とも', 'tomo');
      b1.inOrder(volitional, tomo, 3);
      b1.captureSpan('とも', volitional, tomo);
    },

    // Pattern 2: I-adjective stem (ku-form) + とも (e.g., 辛くとも)
    (b2) => {
      const adj = b2.adj({ inflectionForm: '連用形-一般' }, 'adj');
      const tomo = b2.particle('とも', 'tomo');
      b2.inOrder(adj, tomo, 3);
      b2.captureSpan('とも', adj, tomo);
    },

    // Pattern 3: Na-adjective + であろう + とも (e.g., 下手であろうとも, 好きであろうとも)
    (b3) => {
      const adj = b3.adj({ pos: 'NOUN' }, 'adj');
      const dearou = b3.tok({ text: 'であろう', posOneOf: ['AUX', 'VERB'] }, 'dearou');
      const tomo = b3.particle('とも', 'tomo');
      b3.inOrder(adj, dearou, 3);
      b3.inOrder(dearou, tomo, 1);
      b3.captureSpan('とも', adj, tomo);
    },

    // Pattern 4: Negative verb (なく) + とも (e.g., 届かなくとも, しなくとも)
    (b4) => {
      const naku = b4.tok({ text: 'なく', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'naku');
      const tomo = b4.particle('とも', 'tomo');
      b4.inOrder(naku, tomo, 3);
      b4.captureSpan('とも', naku, tomo);
    },
  );
});
