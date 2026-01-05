import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('もらう', (r) => {
  // もらう (morau): to receive, to get, to obtain
  // Main verb meaning "to receive" (not auxiliary て-form construction)
  // Similar to あげる, くれる in the giving/receiving family

  const morau = r.verb({
    lemma: 'もらう',
  }, 'morau');

  r.capture(morau);
});
