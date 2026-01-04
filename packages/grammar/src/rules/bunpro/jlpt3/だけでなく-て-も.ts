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
  const de = r.tok({ text: 'で' }, 'de');
  const nai = r.aux({ lemma: 'ない' }, 'nai');

  r.either(
    // Pattern 1: だけでなく (formal, no て)
    (b) => {
      const mo = b.particle('も', 'mo');
      b.inOrder(dake, de, 1).inOrder(de, nai, 2);
      // も should come after なく, not immediately after
      // The structure is: X+だけでなく、Y+も
      b.captureSpan('だけでなく', dake, nai);
    },

    // Pattern 2: だけでなくて (formal with て)
    (b) => {
      const te = b.aux({ lemma: 'て' }, 'te');
      const mo = b.particle('も', 'mo');
      b.inOrder(dake, de, 1).inOrder(de, nai, 2).inOrder(nai, te, 1);
      b.captureSpan('だけでなくて', dake, te);
    },

    // Pattern 3: だけではなく (written form, no て)
    (b) => {
      const deParticle = b.particle({ textOneOf: ['は', 'も'] }, 'deParticle');
      const mo = b.particle('も', 'mo');
      b.caseMarker(nai, deParticle);
      b.inOrder(dake, de, 1).inOrder(de, nai, 2);
      b.captureSpan('だけではなく', dake, nai);
    },

    // Pattern 4: だけではなくて (written form with て)
    (b) => {
      const te = b.aux({ lemma: 'て' }, 'te');
      const deParticle = b.particle({ textOneOf: ['は', 'も'] }, 'deParticle');
      const mo = b.particle('も', 'mo');
      b.caseMarker(nai, deParticle);
      b.inOrder(dake, de, 1).inOrder(de, nai, 2).inOrder(nai, te, 1);
      b.captureSpan('だけではなくて', dake, te);
    },

    // Pattern 5: だけじゃなく (casual, no て)
    (b) => {
      const ja = b.tok({ textOneOf: ['じゃ', 'では'] }, 'ja');
      const mo = b.particle('も', 'mo');
      b.inOrder(dake, ja, 1).inOrder(ja, nai, 2);
      b.captureSpan('だけじゃなく', dake, nai);
    },

    // Pattern 6: だけじゃなくて (casual with て)
    (b) => {
      const ja = b.tok({ textOneOf: ['じゃ', 'では'] }, 'ja');
      const te = b.aux({ lemma: 'て' }, 'te');
      const mo = b.particle('も', 'mo');
      b.inOrder(dake, ja, 1).inOrder(ja, nai, 2).inOrder(nai, te, 1);
      b.captureSpan('だけじゃなくて', dake, te);
    }
  );
});
