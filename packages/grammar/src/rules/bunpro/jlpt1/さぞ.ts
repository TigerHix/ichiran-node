import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('さぞ', (r) => {
  // さぞ - adverb expressing conjecture (surely, probably, must be)
  // Key discriminator: GiNZA parses さぞ as NOUN (not ADV like other adverbs)
  // Often followed by conjecture endings: だろう, でしょう, に違いない

  // Variants:
  // 1. さぞ alone (pos=NOUN)
  // 2. さぞかし (single ADV token)
  // 3. さぞや (さぞ NOUN + や ADP)

  r.either(
    (b) => {
      // さぞ or さぞや - followed by conjecture
      // GiNZA parses as either NOUN or VERB depending on context
      const sazo = b.tok({ text: 'さぞ', posOneOf: ['NOUN', 'VERB'] }, 'sazo');

      // Optional particle や for さぞや variant
      b.optional((ob) => {
        const ya = ob.particle('や', 'ya');
        ob.inOrder(sazo, ya, 1);
      });

      // Capture from さぞ (with optional や)
      b.capture(sazo);
    },
    (b) => {
      // さぞかし - single token variant
      const sazokashi = b.tok({ text: 'さぞかし', pos: 'ADV' }, 'sazo');
      b.capture(sazokashi);
    }
  );
});
