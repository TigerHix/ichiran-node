import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('がいる', (r) => {
  r.either(
    // Pattern 1: Simple form (いる)
    (b) => {
      const ga = b.particle('が', 'ga');
      const iru = b.tok({ lemma: 'いる', pos: 'VERB' }, 'iru');
      b.inOrder(ga, iru, 1);
      b.captureSpan('がいる', ga, iru);
    },
    // Pattern 2: Polite form (います)
    (b) => {
      const ga = b.particle('が', 'ga');
      const iru = b.tok({ lemma: 'いる', pos: 'VERB' }, 'iru');
      const masu = b.tok({ lemma: 'ます', pos: 'AUX' }, 'masu');
      b.auxOf(iru, masu);
      b.inOrder(ga, iru, 1);
      b.captureSpan('がいる', ga, masu);
    }
  );
});
