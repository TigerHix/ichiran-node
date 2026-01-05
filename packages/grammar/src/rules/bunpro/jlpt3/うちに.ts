import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('うちに', (r) => {
  // うちに attaches to:
  // 1. [い]Adjective + うちに
  // 2. Verb［ている］+ うちに
  // 3. ［な］Adjective + な + うちに
  // 4. Noun + の + うちに
  // 5. Adverb + うちに (e.g., みるみるうちに)

  // Key: うち is a temporal noun (NOUN, dep=obl)
  // with case marker に (ADP, dep=case) pointing to it
  const uchi = r.noun({ lemma: 'うち', dep: 'obl' }, 'uchi');
  const ni = r.particle('に', 'ni', { dep: 'case' });

  r.caseMarker(uchi, ni);

  r.either(
    // Pattern 1: い-adjective + うちに (近いうちに, 熱いうちに, 明るいうちに)
    (b) => {
      const adj = b.adj({ dep: 'acl' }, 'adj');
      b.inOrder(adj, uchi, 1);
      b.captureSpan('うちに', adj, ni);
    },
    // Pattern 2: な-adjective + な + うちに (元気なうちに)
    (b) => {
      const naAdj = b.adj({ dep: 'acl' }, 'naAdj');
      const na = b.aux({ lemma: 'だ', dep: 'aux' }, 'na');
      b.inOrder(naAdj, na, 1);
      b.inOrder(na, uchi, 1);
      b.captureSpan('うちに', naAdj, ni);
    },
    // Pattern 3: Verb (in te-form) + いる + うちに (食べているうちに, 日本にいるうちに)
    // The verb before て has dep=acl pointing to うち
    (b) => {
      const verbTe = b.tok({ posOneOf: ['VERB', 'AUX'], dep: 'acl' }, 'verbTe');
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b.verb({ lemma: 'いる', dep: 'fixed' }, 'iru');
      b.inOrder(verbTe, te, 3); // Allow for auxiliaries between verb and て
      b.inOrder(te, iru, 1);
      b.inOrder(iru, uchi, 1);
      b.captureSpan('うちに', verbTe, ni);
    },
    // Pattern 4: Verb + うちに (いるうちに - when いる itself modifies うち)
    (b) => {
      const verb = b.verb({ dep: 'acl' }, 'verb');
      b.inOrder(verb, uchi, 1);
      b.captureSpan('うちに', verb, ni);
    },
    // Pattern 5: Noun + の + うちに (一晩のうちに, 一生のうちに, 今のうちに)
    (b) => {
      const noun = b.noun({ dep: 'nmod' }, 'noun');
      const no = b.particle('の', 'no', { dep: 'case' });
      b.inOrder(noun, no, 1);
      b.inOrder(no, uchi, 1);
      b.captureSpan('うちに', noun, ni);
    },
    // Pattern 6: Adverb + うちに (みるみるうちに)
    (b) => {
      const adv = b.adv({ dep: 'advmod' }, 'adv');
      b.inOrder(adv, uchi, 1);
      b.captureSpan('うちに', adv, ni);
    }
  );
});
