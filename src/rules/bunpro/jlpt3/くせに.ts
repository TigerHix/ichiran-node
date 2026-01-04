import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('くせに', (r) => {
  // くせに - "despite/even though" with critical/complaining nuance
  // Patterns:
  // 1. Verb/Adj + くせに (direct attachment)
  // 2. Noun + のくせに (no + kuseni)
  // 3. な-adj + なくせに (na + kuseni)
  //
  // くせに is a noun (癖) + case marker に, used as a conjunction
  // It attaches to attributive forms and expresses criticism/contempt

  const kuseni = r.tok({ text: 'くせに', lemma: 'くせに' }, 'kuseni');

  r.either(
    // Pattern 1: Verb + くせに (attributive form)
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(verb, kuseni, 1);
      b.captureSpan('くせに', verb, kuseni);
    },
    // Pattern 2: い-adjective + くせに (attributive form)
    (b) => {
      const adj = b.adj({ pos: 'ADJ' }, 'adj');
      b.inOrder(adj, kuseni, 1);
      b.captureSpan('くせに', adj, kuseni);
    },
    // Pattern 3: な-adjective + なくせに (na + kuseni)
    (b) => {
      const naAdj = b.adj({}, 'naAdj');
      const na = b.aux({ text: 'な' }, 'na');
      b.inOrder(naAdj, na, 1);
      b.inOrder(na, kuseni, 1);
      b.captureSpan('なくせに', naAdj, kuseni);
    },
    // Pattern 4: Noun + のくせに (no + kuseni)
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);
      b.inOrder(no, kuseni, 1);
      b.captureSpan('のくせに', noun, kuseni);
    }
  );
});
