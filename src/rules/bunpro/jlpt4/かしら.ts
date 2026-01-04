import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('かしら', (r) => {
  // かしら - feminine sentence-final particle expressing wonder/uncertainty
  // Must be at sentence end (followed by punctuation or another particle)
  r.either(
    (branch1) => {
      // Case 1: かしら followed by PUNCT (end of sentence)
      const kashira = branch1.tok({ text: 'かしら', pos: 'PART', dep: 'mark' }, 'kashira');
      const punct = branch1.tok({ pos: 'PUNCT' });
      branch1.inOrder(kashira, punct, 1);
      branch1.capture(kashira);
    },
    (branch2) => {
      // Case 2: かしら at very end of sentence (no following tokens)
      const kashira = branch2.tok({ text: 'かしら', pos: 'PART', dep: 'mark' }, 'kashira');
      branch2.capture(kashira);
    }
  );
});
