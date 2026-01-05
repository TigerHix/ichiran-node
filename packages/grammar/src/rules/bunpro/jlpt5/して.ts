import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('して', (r) => {
  // Match して (te-form of する) used as conjunction
  // GiNZA parses as: verb + "し" (AUX) + "て" (SCONJ)
  // Both "し" and "て" are attached to the same verb (not to each other)
  const verb = r.verb({}, 'verb');

  const shi = r.aux(
    {
      text: 'し',
      lemma: 'する',
    },
    'shi'
  );

  const te = r.tok(
    {
      text: 'て',
      lemma: 'て',
      pos: 'SCONJ',
    },
    'te'
  );

  r.auxOf(verb, shi);
  r.headChild(verb, te, 'mark');
  r.inOrder(shi, te, 1);

  r.captureSpan('して', shi, te);
});
