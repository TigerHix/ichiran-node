import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('がある-noun', (r) => {
  // This rule matches がある when it modifies a following noun (relative clause)
  // e.g., "ベッドがある部屋" (a room that has a bed)
  // This is distinct from がある at the end of a sentence

  r.either(
    // Pattern 1: Simple form (ある)
    (b) => {
      const ga = b.particle('が', 'ga');
      const aru = b.tok({ lemma: 'ある', pos: 'VERB', dep: 'acl' }, 'aru');
      b.inOrder(ga, aru, 1);

      // Key: ある must be followed by a noun (the modified noun)
      const noun = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      b.inOrder(aru, noun);

      b.captureSpan('がある-noun', ga, noun);
    },
    // Pattern 2: Polite form (あります)
    (b) => {
      const ga = b.particle('が', 'ga');
      const aru = b.tok({ lemma: 'ある', pos: 'VERB', inflectionForm: '連用形-一般', dep: 'acl' }, 'aru');
      const masu = b.tok({ lemma: 'ます', pos: 'AUX' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(ga, aru, 1);

      // Key: ます must be followed by a noun
      const noun = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      b.inOrder(masu, noun);

      b.captureSpan('がある-noun', ga, noun);
    }
  );
});
