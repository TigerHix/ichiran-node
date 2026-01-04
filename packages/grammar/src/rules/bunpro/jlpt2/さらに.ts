import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('さらに', (r) => {
  // さらに (sarani) - adverb meaning "furthermore, additionally, even more, again"
  // Patterns:
  // - さらに + verb: さらに詳しく説明します, さらに増えた, さらに高くなった
  // - さらに + i-adjective: さらに美味しい, さらに遅い, さらに激しい
  // - さらに + na-adjective: さらに厳しくなった (actually verb)
  // - 更に + verb (kanji form): same patterns

  // Accept さらに with any POS - GiNZA may tag it variably
  const sarani = r.tok({
    textOneOf: ['さらに', '更に'],
  }, 'sarani');

  r.either(
    // Pattern 1: さらに + verb (most common pattern)
    // さらに詳しく説明します, さらに増えた, さらに高くなった
    // さらに進める, さらに強化できれば, さらに乾燥がひどくなり
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(sarani, verb, 2);
      b.captureSpan('さらに', sarani, verb);
    },

    // Pattern 2: さらに + i-adjective
    // さらに美味しい, さらに遅い, さらに激しい
    (b) => {
      const adj = b.adj({
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'adj');
      b.inOrder(sarani, adj, 2);
      b.captureSpan('さらに', sarani, adj);
    },

    // Pattern 3: さらに + na-adjective
    // Used with na-adjectives directly in certain contexts
    (b) => {
      const adj = b.adj({
        tag: '形状詞-一般',
      }, 'adj');
      b.inOrder(sarani, adj, 2);
      b.captureSpan('さらに', sarani, adj);
    },

    // Pattern 4: さらに + noun (adverbial use with nouns)
    // さらに行列 (rarer but possible)
    (b) => {
      const noun = b.noun({}, 'noun');
      b.inOrder(sarani, noun, 2);
      b.captureSpan('さらに', sarani, noun);
    }
  );
});
