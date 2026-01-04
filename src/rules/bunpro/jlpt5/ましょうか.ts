import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ましょうか', (r) => {
  // ましょうか (mashou ka): shall we, shall I
  // Polite expression for suggesting mutual activities, with indirect invitation nuance
  // Verb stem + ましょうか

  r.either(
    // Pattern 1: Regular verbs with stem form
    (b1) => {
      const verb = b1.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mashou = b1.aux({
        lemma: 'ます',
        inflectionForm: '意志推量形',
      }, 'mashou');
      const ka = b1.particle('か', 'ka', { pos: 'PART', depOneOf: ['discourse', 'mark'] });

      b1.auxOf(verb, mashou);
      b1.inOrder(mashou, ka, 2);
      b1.captureSpan('ましょうか', verb, ka);
    },

    // Pattern 2: Suru-verbs (noun verb + し + ましょうか)
    (b2) => {
      const nounVerb = b2.verb({}, 'verb');
      const shi = b2.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般',
      }, 'shi');
      const mashou = b2.aux({
        lemma: 'ます',
        inflectionForm: '意志推量形',
      }, 'mashou');
      const ka = b2.particle('か', 'ka', { pos: 'PART', depOneOf: ['discourse', 'mark'] });

      b2.auxOf(nounVerb, shi);
      b2.auxOf(nounVerb, mashou);
      b2.inOrder(mashou, ka, 2);
      b2.captureSpan('ましょうか', nounVerb, ka);
    }
  );
});
