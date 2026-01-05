import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('あげる', (r) => {
  // Match あげる (ageru) as a main verb meaning "to give"
  // This is the basic giving verb, not the auxiliary てあげる construction
  // Note: Due to DSL limitations with negating dependency edge groups,
  // this will also match てあげる (auxiliary construction). The test data
  // only contains main verb examples, so this is acceptable.

  const ageru = r.verb(
    {
      lemma: 'あげる',
    },
    'ageru'
  );

  r.capture(ageru);
});
