import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('がみられる', (r) => {
  r.either(
    // Pattern 1: Standard potential form as single token (みられる)
    (b) => {
      const ga = b.tok({ textOneOf: ['が', 'も'], pos: 'ADP' }, 'ga');
      const mirareru = b.verb({ lemmaOneOf: ['見られる', 'みられる'] }, 'mirareru');
      b.inOrder(ga, mirareru, 1);
      b.captureSpan('がみられる', ga, mirareru);
    },
    // Pattern 2: Standard potential form without explicit particle (subject omitted)
    (b) => {
      const mirareru = b.verb({ lemmaOneOf: ['見られる', 'みられる'] }, 'mirareru');
      b.captureSpan('がみられる', mirareru, mirareru);
    },
    // Pattern 3: Colloquial potential form as single token (みれる)
    (b) => {
      const ga = b.tok({ textOneOf: ['が', 'も'], pos: 'ADP' }, 'ga');
      const mireru = b.verb({ lemmaOneOf: ['見れる', 'みれる'] }, 'mireru');
      b.inOrder(ga, mireru, 1);
      b.captureSpan('がみられる', ga, mireru);
    },
    // Pattern 4: Standard potential form split across tokens with particle (が + 見 + られる)
    (b) => {
      const ga = b.tok({ textOneOf: ['が', 'も'], pos: 'ADP' }, 'ga');
      const miru = b.tok({ textOneOf: ['見', 'み'], posOneOf: ['VERB', 'NOUN'] }, 'miru');
      const reru = b.aux({ textOneOf: ['られる', 'られ'] }, 'reru');
      b.auxOf(miru, reru);
      b.inOrder(ga, miru, 3);
      b.captureSpan('がみられる', ga, reru);
    },
    // Pattern 5: Potential form split across tokens without particle or with non-object particle (見 + られる, subject marked with は/に/で/etc or omitted)
    (b) => {
      const miru = b.tok({ textOneOf: ['見', 'み'], posOneOf: ['VERB', 'NOUN'] }, 'miru');
      const reru = b.aux({ textOneOf: ['られる', 'られ'] }, 'reru');
      b.auxOf(miru, reru);
      b.captureSpan('がみられる', miru, reru);
    },
    // Pattern 6: Polite potential form as single token (みられます)
    (b) => {
      const ga = b.tok({ textOneOf: ['が', 'も'], pos: 'ADP' }, 'ga');
      const miraremasu = b.verb({ lemmaOneOf: ['見られます', 'みられます'] }, 'miraremasu');
      b.inOrder(ga, miraremasu, 1);
      b.captureSpan('がみられる', ga, miraremasu);
    },
    // Pattern 7: Polite potential form split across tokens (見る + られる + ます)
    (b) => {
      const ga = b.tok({ textOneOf: ['が', 'も'], pos: 'ADP' }, 'ga');
      const miru = b.tok({ textOneOf: ['見', 'み'], posOneOf: ['VERB', 'NOUN'] }, 'miru');
      const reru = b.aux({ textOneOf: ['られる', 'られ'] }, 'reru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(miru, reru);
      b.auxOf(reru, masu);
      b.inOrder(ga, miru, 4);
      b.captureSpan('がみられる', ga, masu);
    }
  );
});
