import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ながら', (r) => {
  // Verb masu stem + ながら (while doing X)
  // Use either() to handle two patterns:
  // 1. Standard: verb with inflectionForm=連用形-一般 (e.g., 聴き, 食べ, し)
  // 2. Edge case: verbs with inflectionForm=undefined (GiNZA quirk for hiragana-only text like ね)
  r.either(
    // Pattern 1: Standard verb stems with inflectionForm
    (branch1) => {
      const verbStem = branch1.tok({
        inflectionForm: '連用形-一般',
        posOneOf: ['VERB', 'AUX'],
        depOneOf: ['advcl', 'aux', 'root', 'obj', 'obl']
      }, 'verbStem');

      const nagara = branch1.tok({ lemma: 'ながら', pos: 'SCONJ' }, 'nagara');

      branch1.inOrder(verbStem, nagara, 1);
      branch1.captureSpan('ながら', verbStem, nagara);
    },

    // Pattern 2: Edge case for hiragana-only verbs without inflectionForm
    // e.g., ね from 寝る (GiNZA parses as lemma=ね, inflectionForm=undefined)
    // Must still have dep=advcl to ensure it's a verb stem in adverbial clause
    (branch2) => {
      const verbStem = branch2.verb({
        dep: 'advcl'
      }, 'verbStem');

      const nagara = branch2.tok({ lemma: 'ながら', pos: 'SCONJ' }, 'nagara');

      // Exclude verbs that have inflectionForm (those are handled by branch 1)
      branch2.not((n) => {
        n.verb({
          inflectionForm: '連用形-一般'
        }, 'verbStem');
      });

      branch2.inOrder(verbStem, nagara, 1);
      branch2.captureSpan('ながら', verbStem, nagara);
    }
  );
});
