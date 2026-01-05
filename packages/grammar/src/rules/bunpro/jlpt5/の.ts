import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('の', (r) => {
  // の as possessive particle / nominalizer
  // Structure: [NOUN/PROPN/PRON] の [NOUN/PROPN/PRON/VERB/ADJ]
  // or: [VERB/ADJ] の (nominalization)

  const no = r.particle('の', 'no', { dep: 'case' });

  // Noun + の + noun (possession/attribute)
  r.either(
    // Possessive: noun の noun
    (b1) => {
      const noun1 = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'DET', 'NUM'] }, 'noun1');
      const noun2 = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'DET', 'NUM'] }, 'noun2');

      b1.caseMarker(noun1, no);
      b1.inOrder(no, noun2, 3);
      b1.capture(no);
    },
    // Nominalizer: verb/adj の (turns phrase into noun)
    (b2) => {
      const content = b2.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'content');

      b2.inOrder(content, no, 2);
      b2.capture(no);
    }
  );
});
