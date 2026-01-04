import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('じゃあるまいし', (r) => {
  // Core pattern: ある + まい + し (always present, consecutive)
  const aru = r.verb({ lemma: 'ある' }, 'aru');
  const mai = r.tok({ text: 'まい' }, 'mai');
  const shi = r.tok({ text: 'し', pos: 'SCONJ' }, 'shi');
  r.inOrder(aru, mai, 1).inOrder(mai, shi, 1);

  // Variants for the prefix: じゃ / でも / では
  r.either(
    (b) => {
      // じゃあるまいし (じゃ has lemma=だ, pos=AUX)
      const ja = b.tok({ text: 'じゃ', pos: 'AUX' }, 'ja');
      b.inOrder(ja, aru, 1);
      b.captureSpan('じゃあるまいし', ja, shi);
    },
    (b) => {
      // でもあるまいし (で + も)
      const de = b.tok({ lemma: 'で', posOneOf: ['AUX', 'ADP'] }, 'ja');
      const mo = b.tok({ text: 'も' }, 'mo');
      b.inOrder(de, mo, 1).inOrder(mo, aru, 1);
      b.captureSpan('じゃあるまいし', de, shi);
    },
    (b) => {
      // ではあるまいし (で + は)
      const de = b.tok({ text: 'で', posOneOf: ['AUX', 'ADP'] }, 'ja');
      const wa = b.tok({ text: 'は' }, 'wa');
      b.inOrder(de, wa, 1).inOrder(wa, aru, 1);
      b.captureSpan('じゃあるまいし', de, shi);
    }
  );
});

