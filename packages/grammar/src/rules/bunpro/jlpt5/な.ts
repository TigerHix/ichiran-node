import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('な', (r) => {
  // Prohibitive particle な (verb + な = "don't do!")
  // Must be pos=PART (not AUX which is for na-adjective copula)
  // Must have dep=mark (sentence-ending particle marker)
  // Must follow a verb (dictionary form)
  // Must be at end of sentence (followed by punctuation or another particle)

  const na = r.tok({ text: 'な', pos: 'PART', dep: 'mark' }, 'na');

  r.either(
    (branch1) => {
      // Case 1: な followed by PUNCT (end of sentence)
      const punct = branch1.tok({ pos: 'PUNCT' });
      branch1.inOrder(na, punct, 1);
      branch1.capture(na);
    },
    (branch2) => {
      // Case 2: な followed by another sentence particle
      const particle = branch2.tok({ pos: 'PART' });
      branch2.inOrder(na, particle, 1);
      branch2.capture(na);
    }
  );
});
