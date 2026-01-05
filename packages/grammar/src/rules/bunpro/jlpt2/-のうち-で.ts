import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('-のうち-で', (r) => {
  // Match うち used partitively (のうち, このうち, or standalone うち)
  // うち can have dep: obl, nmod, compound
  r.either(
    (b) => {
      // のうち pattern
      const no = b.particle('の', 'no');
      const uchi = b.tok({ lemma: 'うち', pos: 'NOUN' }, 'uchi');
      b.inOrder(no, uchi, 1);
      b.captureSpan('のうち', no, uchi);
    },
    (b) => {
      // standalone うち followed by number (うち６名 etc.)
      const uchi = b.tok({ lemma: 'うち', pos: 'NOUN', depOneOf: ['nmod', 'compound'] }, 'uchi');
      const num = b.tok({ pos: 'NUM' }, 'num');
      b.inOrder(uchi, num, 2);
      b.captureSpan('うち', uchi, num);
    }
  );
});

