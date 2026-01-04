import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: が気になる (ga ki ni naru) - "to be concerned about, to be interested in"
 */
export default linguisticRule('が気になる', (r) => {
  r.either(
    // Pattern 1: 気 + に + なる (casual form)
    (b1) => {
      const ki = b1.noun({ lemmaOneOf: ['気', 'き'] }, 'ki');
      const ni = b1.particle('に', 'ni');
      const naru = b1.verb({ lemma: 'なる' }, 'naru');

      b1.inOrder(ki, ni, 1);
      b1.inOrder(ni, naru, 1);

      b1.caseMarker(ki, ni);
      b1.auxOf(naru, ni);

      b1.captureSpan('が気になる', ki, naru);
    },

    // Pattern 2: 気 + に + なります (polite form)
    (b2) => {
      const ki = b2.noun({ lemmaOneOf: ['気', 'き'] }, 'ki');
      const ni = b2.particle('に', 'ni');
      const naru = b2.verb({ lemma: 'なる' }, 'naru');
      const masu = b2.aux({ lemma: 'ます' }, 'masu');

      b2.inOrder(ki, ni, 1);
      b2.inOrder(ni, naru, 1);
      b2.inOrder(naru, masu, 1);

      b2.caseMarker(ki, ni);
      b2.auxOf(naru, ni);
      b2.auxOf(masu, naru);

      b2.captureSpan('が気になります', ki, masu);
    },

    // Pattern 3: 気 + に + なっている (progressive/state, casual)
    (b3) => {
      const ki = b3.noun({ lemmaOneOf: ['気', 'き'] }, 'ki');
      const ni = b3.particle('に', 'ni');
      const natte = b3.aux({ lemma: 'なる' }, 'natte');
      const iru = b3.aux({ lemma: 'いる' }, 'iru');

      b3.inOrder(ki, ni, 1);
      b3.inOrder(ni, natte, 1);
      b3.inOrder(natte, iru, 1);

      b3.caseMarker(ki, ni);
      b3.auxOf(natte, ni);
      b3.auxOf(iru, natte);

      b3.captureSpan('が気になっている', ki, iru);
    },

    // Pattern 4: 気 + に + なっています (progressive/state, polite)
    (b4) => {
      const ki = b4.noun({ lemmaOneOf: ['気', 'き'] }, 'ki');
      const ni = b4.particle('に', 'ni');
      const natte = b4.aux({ lemma: 'なる' }, 'natte');
      const iru = b4.aux({ lemma: 'いる' }, 'iru');
      const masu = b4.aux({ lemma: 'ます' }, 'masu');

      b4.inOrder(ki, ni, 1);
      b4.inOrder(ni, natte, 1);
      b4.inOrder(natte, iru, 1);
      b4.inOrder(iru, masu, 1);

      b4.caseMarker(ki, ni);
      b4.auxOf(natte, ni);
      b4.auxOf(iru, natte);
      b4.auxOf(masu, iru);

      b4.captureSpan('が気になっています', ki, masu);
    }
  );
});
