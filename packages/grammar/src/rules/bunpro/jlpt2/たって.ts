import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('たって', (r) => {
  r.either(
    // Pattern 1: Verb ta-form + ってtte (e.g., 聞いたって, 謝ったって)
    (b1) => {
      const verbTa = b1.tok({ inflectionForm: 'タ形-一般' }, 'verbTa');
      const tte = b1.tok({ text: 'って', pos: 'PART' }, 'tte');
      b1.inOrder(verbTa, tte, 3);
      b1.captureSpan('たって', verbTa, tte);
    },

    // Pattern 2: Verb naku-form + たって (e.g., なくたって, 楽しくなくたって)
    (b2) => {
      const naku = b2.tok({ text: 'なく', pos: 'AUX' }, 'naku');
      const tatte = b2.tok({ text: 'たって', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tatte');
      b2.inOrder(naku, tatte, 3);
      b2.captureSpan('たって', naku, tatte);
    },

    // Pattern 3: I-adjective ku-form + たって (e.g., よくたって, 欲しくたって)
    (b3) => {
      const iAdjKu = b3.tok({ inflectionForm: '連用形-一般' }, 'iAdjKu');
      const tatte = b3.tok({ text: 'たって', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tatte');
      b3.inOrder(iAdjKu, tatte, 3);
      b3.captureSpan('たって', iAdjKu, tatte);
    },

    // Pattern 4: Noun/Na-adjective + だって (e.g., 馬鹿だって, 友達だって)
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const datte = b4.tok({ text: 'だって', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'datte');
      b4.inOrder(noun, datte, 3);
      b4.captureSpan('たって', noun, datte);
    },
  );
});
