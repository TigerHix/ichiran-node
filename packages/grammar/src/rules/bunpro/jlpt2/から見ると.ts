import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('から見ると', (r) => {
  const noun = r.tok({
    posOneOf: ['NOUN', 'PROPN', 'PRON'],
  }, 'noun');

  const kara = r.particle('から', 'kara');
  r.inOrder(noun, kara, 1);

  r.either(
    // Pattern 1: から + みる/見る + と (から見ると/からみると)
    (b1) => {
      const miru = b1.verb({ lemmaOneOf: ['みる', '見る'] }, 'miru');
      const to = b1.tok({ text: 'と' }, 'to');
      b1.inOrder(kara, miru, 5);
      b1.inOrder(miru, to, 1);
      b1.captureSpan('から見ると', noun, to);
    },
    // Pattern 2: から + み/見 + たら (から見たら/からみたら)
    (b2) => {
      const mira = b2.tok({ textOneOf: ['見', 'み'], lemmaOneOf: ['見る', 'みる'] }, 'mira');
      const tara = b2.tok({ text: 'たら' }, 'tara');
      b2.inOrder(kara, mira, 5);
      b2.inOrder(mira, tara, 1);
      b2.captureSpan('から見ると', noun, tara);
    },
    // Pattern 3: から + みれ/見れ + ば (から見れば/からみれば)
    (b3) => {
      const mirure = b3.tok({ textOneOf: ['見れ', 'みれ'], lemmaOneOf: ['見る', 'みる'] }, 'mirure');
      const ba = b3.tok({ text: 'ば', pos: 'SCONJ' }, 'ba');
      b3.inOrder(kara, mirure, 5);
      b3.inOrder(mirure, ba, 1);
      b3.captureSpan('から見ると', noun, ba);
    },
    // Pattern 4a: から + みて/見て as single token (から見て/からみて)
    (b4a) => {
      const mite = b4a.aux({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般'
      }, 'mite');
      b4a.inOrder(kara, mite, 5);
      b4a.captureSpan('から見ると', noun, mite);
    },
    // Pattern 4b: から + み/見 + て as two tokens (から見て/からみて)
    (b4b) => {
      const mi = b4b.tok({ textOneOf: ['見', 'み'], lemmaOneOf: ['見る', 'みる'] }, 'mi');
      const te = b4b.tok({ text: 'て' }, 'te');
      b4b.inOrder(kara, mi, 5);
      b4b.inOrder(mi, te, 1);
      b4b.captureSpan('から見ると', noun, te);
    }
  );
});
