import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('から見ると', (r) => {
  const noun = r.tok({
    posOneOf: ['NOUN', 'PROPN', 'PRON'],
  }, 'noun');

  const kara = r.particle('から', 'kara');
  r.inOrder(noun, kara, 1);

  r.either(
    (b1) => {
      const miru = b1.verb({ lemmaOneOf: ['みる', '見る'] }, 'miru');
      const to = b1.tok({ text: 'と' }, 'to');
      b1.inOrder(kara, miru, 5);
      b1.inOrder(miru, to, 1);
      b1.captureSpan('から見ると', noun, to);
    },
    (b2) => {
      const mira = b2.tok({ textOneOf: ['見', 'み'], lemmaOneOf: ['見る', 'みる'] }, 'mira');
      const tara = b2.tok({ text: 'たら' }, 'tara');
      b2.inOrder(kara, mira, 5);
      b2.inOrder(mira, tara, 1);
      b2.captureSpan('から見ると', noun, tara);
    },
    (b3) => {
      const mirure = b3.tok({ textOneOf: ['見れ', 'みれ'], lemmaOneOf: ['見る', 'みる'] }, 'mirure');
      const ba = b3.tok({ text: 'ば', pos: 'SCONJ' }, 'ba');
      b3.inOrder(kara, mirure, 5);
      b3.inOrder(mirure, ba, 1);
      b3.captureSpan('から見ると', noun, ba);
    },
    (b4) => {
      const mite = b4.aux({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般'
      }, 'mite');
      b4.inOrder(kara, mite, 5);
      b4.captureSpan('から見ると', noun, mite);
    }
  );
});
