import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('くせに', (r) => {
  // くせに - "despite/even though" with critical/complaining nuance
  // GiNZA tokenizes くせに as two tokens: くせ (noun) + に (particle)
  // Patterns:
  // 1. Verb stem + くせに (verb stem + くせ + に)
  // 2. い-adjective + くせに (adj + くせ + に)
  // 3. な-adjective + な + くせに (adj + な + くせ + に)
  // 4. Noun + のくせに (noun + の + くせ + に)

  r.either(
    // Pattern 1: Verb + くせに (attributive form + くせ + に)
    (b) => {
      const verb = b.verb({}, 'verb');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(verb, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', verb, ni);
    },
    // Pattern 2: い-adjective + くせに (attributive form + くせ + に)
    (b) => {
      const adj = b.adj({}, 'adj');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(adj, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', adj, ni);
    },
    // Pattern 3: な-adjective + なくせに (na + くせ + に)
    (b) => {
      const naAdj = b.adj({}, 'naAdj');
      const na = b.aux({ text: 'な' }, 'na');
      b.inOrder(naAdj, na, 1);
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(na, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('なくせに', naAdj, ni);
    },
    // Pattern 4: Noun + のくせに (noun + の + くせ + に)
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
    // Pattern 5: がる verb suffix + なくせに (special merged form)
    // After がる, "な + くせ" gets merged into "なくせ"
    (b) => {
      const garu = b.tok({ lemmaOneOf: ['がる', 'がり'], tag: '接尾辞-動詞的' }, 'garu');
      const nakuse = b.tok({ text: 'なくせ', pos: 'NOUN' }, 'nakuse');
      b.inOrder(garu, nakuse, 10);
      const ni = b.particle('に', 'ni');
      b.inOrder(nakuse, ni, 1);
      b.captureSpan('くせに', garu, ni);
    }
  );
});
