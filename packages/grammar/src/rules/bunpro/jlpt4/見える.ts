import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('見える', (r) => {
  r.either(
    // Pattern 1: Subject + が + 見える (standard form with subject marker)
    (b) => {
      const ga = b.tok({ textOneOf: ['が', 'も'], pos: 'ADP' }, 'ga');
      const mieru = b.verb({ lemmaOneOf: ['見える', 'みえる'] }, 'mieru');
      b.inOrder(ga, mieru, 1);
      b.captureSpan('見える', ga, mieru);
    },
    // Pattern 2: 見える without explicit particle (subject omitted or contextually understood)
    (b) => {
      const mieru = b.verb({ lemmaOneOf: ['見える', 'みえる'] }, 'mieru');
      b.captureSpan('見える', mieru, mieru);
    },
    // Pattern 3: Subject + が + 見えない (negative form)
    (b) => {
      const ga = b.tok({ textOneOf: ['が', 'も'], pos: 'ADP' }, 'ga');
      const mienai = b.verb({ lemmaOneOf: ['見えない', 'みえない'] }, 'mienai');
      b.inOrder(ga, mienai, 1);
      b.captureSpan('見える', ga, mienai);
    },
    // Pattern 4: 見えない without explicit particle
    (b) => {
      const mienai = b.verb({ lemmaOneOf: ['見えない', 'みえない'] }, 'mienai');
      b.captureSpan('見える', mienai, mienai);
    },
    // Pattern 5: Adverb + 見える (e.g., よく見える, はっきりと見える)
    (b) => {
      const mieru = b.verb({ lemmaOneOf: ['見える', 'みえる'] }, 'mieru');
      b.captureSpan('見える', mieru, mieru);
    },
    // Pattern 6: Adverb + 見えない (e.g., よく見えない)
    (b) => {
      const mienai = b.verb({ lemmaOneOf: ['見えない', 'みえない'] }, 'mienai');
      b.captureSpan('見える', mienai, mienai);
    },
    // Pattern 7: Polite form - 見えます (with or without particle)
    (b) => {
      const mieru = b.verb({ lemmaOneOf: ['見えます', 'みえます'] }, 'mieru');
      b.captureSpan('見える', mieru, mieru);
    },
    // Pattern 8: Polite negative - 見えません (with or without particle)
    (b) => {
      const mieru = b.verb({ lemmaOneOf: ['見えません', 'みえません'] }, 'mieru');
      b.captureSpan('見える', mieru, mieru);
    }
  );
});
