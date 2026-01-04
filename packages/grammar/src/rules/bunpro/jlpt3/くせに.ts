import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('くせに', (r) => {
  // くせに - "despite/even though" with critical/complaining nuance
  // Patterns:
  // 1. Verb/Adj + くせに (direct attachment)
  // 2. Noun + のくせに (no + kuse + ni)
  // 3. な-adj + なくせに (na + kuse + ni)
  //
  // くせ is a noun (癖) + case marker に, used as a conjunction
  // It attaches to attributive forms and expresses criticism/contempt
  //
  // GiNZA may tokenize くせに as either:
  // - Two tokens: くせ/癖 (noun) + に (particle)
  // - Single token: くせに (ADP/SCONJ)

  r.either(
    // Pattern 1: Verb + くせに (two tokens)
    (b) => {
      const verb = b.verb({}, 'verb');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(verb, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', verb, ni);
    },
    // Pattern 2: い-adjective + くせに (two tokens)
    (b) => {
<<<<<<< HEAD
      const adj = b.adj({}, 'adj');
=======
      const adj = b.adj({ pos: 'ADJ' }, 'adj');
>>>>>>> jlpt3-kuse-ni
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(adj, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', adj, ni);
    },
    // Pattern 3: な-adjective + なくせに (two tokens)
    (b) => {
      const naAdj = b.adj({}, 'naAdj');
      const na = b.aux({ text: 'な' }, 'na');
      b.inOrder(naAdj, na, 5);
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(na, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('なくせに', naAdj, ni);
    },
    // Pattern 4: Noun + のくせに (two tokens)
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(no, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('のくせに', noun, ni);
    },
    // Pattern 5: Any + な + くせに (two tokens, catch-all)
    (b) => {
      const na = b.aux({ text: 'な' }, 'na');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(na, kuse, 10);
      b.inOrder(kuse, ni, 1);
      const start = b.tok({}, 'start');
      b.inOrder(start, na, 5);
      b.captureSpan('なくせに', start, ni);
    },
    // Pattern 6: Single token くせに (GiNZA sometimes tokenizes as one)
    (b) => {
      const kuseni = b.tok({ text: 'くせに' }, 'kuseni');
      const prev = b.tok({}, 'prev');
      b.inOrder(prev, kuseni, 10);
      b.captureSpan('くせに', prev, kuseni);
    },
    // Pattern 7: Single token 癖に (kanji version)
    (b) => {
      const kuseni = b.tok({ text: '癖に' }, 'kuseni');
      const prev = b.tok({}, 'prev');
      b.inOrder(prev, kuseni, 10);
      b.captureSpan('くせに', prev, kuseni);
    }
  );
});
