import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だけでなく-て-も', (r) => {
  // だけでなく(て)～も - "not only X but also Y"
  // This rule extends JLPT4's だけでなく by requiring も particle after the second element
  //
  // Patterns:
  // 1. Formal: だけでなく、Noun+も / だけでなくて、Noun+も
  // 2. Written: だけではなく、Noun+も / だけではなくて、Noun+も
  // 3. Casual: だけじゃなく、Noun+も / だけじゃなくて、Noun+も
  //
  // The key difference from JLPT4 だけでなく is the REQUIRED も particle
  // following the second noun/element, emphasizing inclusion of both items.

  const dake = r.tok({ lemma: 'だけ' }, 'dake');
  const nai = r.aux({ lemma: 'ない' }, 'nai');

  r.either(
    // Pattern 1: だけでなく (formal, no て)
    (b) => {
      const deConnector = b.tok({ text: 'で' }, 'de');
      b.inOrder(dake, deConnector, 1).inOrder(deConnector, nai, 2);
      b.captureSpan('pattern', dake, nai);
    },

    // Pattern 2: だけでなくて (formal with て)
    (b) => {
      const deConnector = b.tok({ text: 'で' }, 'de');
      const te = b.aux({ lemma: 'て' }, 'te');
      b.inOrder(dake, deConnector, 1).inOrder(deConnector, nai, 2).inOrder(nai, te, 1);
      b.captureSpan('pattern', dake, te);
    },

    // Pattern 3: だけではなく (written form, no て)
    (b) => {
      const deConnector = b.tok({ text: 'で' }, 'de');
      const waParticle = b.particle('は', 'wa');
      b.inOrder(dake, deConnector, 1).inOrder(deConnector, nai, 2);
      b.caseMarker(nai, waParticle);
      b.captureSpan('pattern', dake, nai);
    },

    // Pattern 4: だけではなくて (written form with て)
    (b) => {
      const deConnector = b.tok({ text: 'で' }, 'de');
      const waParticle = b.particle('は', 'wa');
      const te = b.aux({ lemma: 'て' }, 'te');
      b.inOrder(dake, deConnector, 1).inOrder(deConnector, nai, 2).inOrder(nai, te, 1);
      b.caseMarker(nai, waParticle);
      b.captureSpan('pattern', dake, te);
    },

    // Pattern 5: だけじゃなく (casual 'じゃ' as single token, no て)
    (b) => {
      const ja = b.tok({ textOneOf: ['じゃ', 'じゃあ'] }, 'ja');
      b.inOrder(dake, ja, 1).inOrder(ja, nai, 2);
      b.captureSpan('pattern', dake, nai);
    },

    // Pattern 6: だけじゃなくて (casual 'じゃ' as single token with て)
    (b) => {
      const ja = b.tok({ textOneOf: ['じゃ', 'じゃあ'] }, 'ja');
      const te = b.aux({ lemma: 'て' }, 'te');
      b.inOrder(dake, ja, 1).inOrder(ja, nai, 2).inOrder(nai, te, 1);
      b.captureSpan('pattern', dake, te);
    },

    // Pattern 7: だけではなく (casual 'では', no て)
    (b) => {
      const dewa = b.tok({ text: 'では' }, 'dewa');
      b.inOrder(dake, dewa, 1).inOrder(dewa, nai, 2);
      b.captureSpan('pattern', dake, nai);
    },

    // Pattern 8: だけではなくて (casual 'では' with て)
    (b) => {
      const dewa = b.tok({ text: 'では' }, 'dewa');
      const te = b.aux({ lemma: 'て' }, 'te');
      b.inOrder(dake, dewa, 1).inOrder(dewa, nai, 2).inOrder(nai, te, 1);
      b.captureSpan('pattern', dake, te);
    }
  );
});
