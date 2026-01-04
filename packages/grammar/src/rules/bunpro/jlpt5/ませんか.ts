import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ませんか', (r) => {
  // ませんか (masen ka): won't you, would you not
  // Polite expression for directly inviting someone to do something
  // Verb stem + ませんか

  r.either(
    // Pattern 1: Regular verbs with stem form
    (b1) => {
      const verb = b1.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const masen = b1.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'masen');
      const ka = b1.particle('か', 'ka', { pos: 'PART', depOneOf: ['discourse', 'mark'] });

      b1.auxOf(verb, masen);
      b1.inOrder(masen, ka, 2);
      b1.captureSpan('ませんか', verb, ka);
    },

    // Pattern 2: Suru-verbs (noun verb + し + ませんか)
    (b2) => {
      const nounVerb = b2.verb({}, 'verb');
      const shi = b2.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般',
      }, 'shi');
      const masen = b2.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'masen');
      const ka = b2.particle('か', 'ka', { pos: 'PART', depOneOf: ['discourse', 'mark'] });

      b2.auxOf(nounVerb, shi);
      b2.auxOf(nounVerb, masen);
      b2.inOrder(masen, ka, 2);
      b2.captureSpan('ませんか', nounVerb, ka);
    }
  );
});
