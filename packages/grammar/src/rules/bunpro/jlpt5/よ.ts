import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('よ', (r) => {
  // Sentence-ending particle よ for emphasis
  // Must have dep='mark' and be followed by punctuation (end of sentence)
  // or another sentence particle like ね
  r.either(
    (branch1) => {
      // Case 1: よ followed by PUNCT (end of sentence)
      const yo = branch1.tok({ text: 'よ', pos: 'PART', dep: 'mark' }, 'yo');
      const punct = branch1.tok({ pos: 'PUNCT' });
      branch1.inOrder(yo, punct, 1);
      branch1.capture(yo);
    },
    (branch2) => {
      // Case 2: よ followed by another sentence particle like ね
      const yo = branch2.tok({ text: 'よ', pos: 'PART', dep: 'mark' }, 'yo');
      const particle = branch2.tok({ text: 'ね', pos: 'PART' });
      branch2.inOrder(yo, particle, 1);
      branch2.capture(yo);
    }
  );
});
