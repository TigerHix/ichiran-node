import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('か-or', (r) => {
  // The particle か used to present alternatives (A or B) or as a question marker
  // Matches patterns like:
  // - AかB (A or B): か with pos=ADP, dep=case between content words
  // - Sentence-final か: か with pos=PART, dep=mark at end of sentence

  r.either(
    // Branch 1: か (ADP, dep=case) between alternatives (AかB)
    (branch1) => {
      const ka = branch1.tok({ text: 'か', pos: 'ADP', dep: 'case' }, 'ka');
      const nextWord = branch1.tok({ posOneOf: ['NOUN', 'VERB', 'ADJ', 'PRON'] });
      branch1.inOrder(ka, nextWord, 1); // Immediately follows

      branch1.capture(ka);
    },

    // Branch 2: か (PART, dep=mark) sentence-final
    (branch2) => {
      const ka = branch2.tok({ text: 'か', pos: 'PART', dep: 'mark' }, 'ka');
      const punct = branch2.tok({ pos: 'PUNCT' });
      branch2.inOrder(ka, punct, 5); // Within 5 tokens

      branch2.capture(ka);
    }
  );
});
