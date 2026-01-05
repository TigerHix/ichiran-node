import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('あっての', (r) => {
  // Match Noun (A) + (が) + あって (te-form of ある) + の + Noun (B)
  // The "が" particle between the first noun and あって is optional
  //
  // GiNZA parses あって as:
  //   あっ (lemma=ある, pos=VERB, inflectionForm=連用形-促音便)
  //   て (lemma=て, pos=SCONJ)
  //
  // Key discriminators:
  // - In valid pattern, あっ has dep=nmod (nominal modifier)
  // - In false positive like であって, あっ has dep=fixed (fixed expression)
  //
  // Nouns can be NOUN, PROPN, PRON (for words like 私, 你), or ADJ (words like 幸せ)

  const noun1 = r.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun1');  // First noun (A)

  // "が" is optional
  r.optional((b) => {
    const ga = b.particle('が', 'ga');
    b.inOrder(noun1, ga, 1);
  });

  // あっ (促音便 form of ある) with dep=nmod to exclude であって
  const accu = r.verb({ lemma: 'ある', inflectionForm: '連用形-促音便', dep: 'nmod' }, 'accu');
  const te = r.tok({ text: 'て', pos: 'SCONJ' }, 'te');
  const no = r.particle('の', 'no');
  const noun2 = r.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'] }, 'noun2');  // Second noun (B)

  // Ensure ordering: noun1 -> (ga?) -> accu -> te -> no -> noun2
  r.inOrder(noun1, accu, 2);
  r.inOrder(accu, te, 1);
  r.inOrder(te, no, 1);
  r.inOrder(no, noun2, 1);

  // Capture from first noun to の (includes the whole pattern)
  r.captureSpan('あっての', noun1, no);
});
