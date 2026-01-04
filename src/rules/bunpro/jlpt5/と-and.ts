import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('と-and', (r) => {
  // Match と as "and" (listing particle connecting nouns)
  // This is distinct from:
  // - Quotation と (head has dep=ccomp/advcl/acl/root)
  // - Accompaniment と "with" (head's head is VERB)
  // - Conditional と (SCONJ with dep=mark)
  //
  // Key discriminators:
  // - "and" と: noun1 --[nmod|obl]--> noun2 (where noun2 is NOUN/PROPN/PRON)
  // - "with" と: noun1 --obl--> VERB (noun2 is verb)
  //
  // Structure: NOUN1 と NOUN2, where と marks NOUN1 and NOUN1 points to NOUN2

  const to = r.tok({ text: 'と' }, 'to');

  // The second noun (what noun1 connects to)
  // This is the key discriminator from "with" と (where noun2 would be VERB)
  const noun2 = r.tok({
    posOneOf: ['NOUN', 'PRON', 'PROPN', 'NUM', 'DET']
  }, 'noun2');

  // Two patterns for the first noun:
  r.either(
    // Pattern 1: noun1 has dep=nmod (most common for noun lists)
    (branch1) => {
      const noun1 = branch1.tok({
        posOneOf: ['NOUN', 'PRON', 'PROPN', 'NUM', 'DET'],
        dep: 'nmod'
      }, 'noun1');

      branch1.caseMarker(noun1, to);
      branch1.headChild(noun2, noun1, 'nmod');
      branch1.capture(to);
    },
    // Pattern 2: noun1 has dep=obl (in some predicate structures)
    (branch2) => {
      const noun1 = branch2.tok({
        posOneOf: ['NOUN', 'PRON', 'PROPN', 'NUM', 'DET'],
        dep: 'obl'
      }, 'noun1');

      branch2.caseMarker(noun1, to);
      branch2.headChild(noun2, noun1, 'obl');
      branch2.capture(to);
    }
  );
});
