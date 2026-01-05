import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: だけに (dake ni) - Precisely because, all the more because, as might be expected
 *
 * A grammar pattern meaning "precisely because X", "all the more because X",
 * or "as might be expected from X". It emphasizes that result B is natural or
 * expected specifically because of A. Can express both positive and negative results.
 *
 * Structures:
 * - Verb + だけに
 * - I-adjective + だけに
 * - Na-adjective + なだけに
 * - Noun + だけに
 * - Noun + だ/である + だけに
 *
 * Examples:
 * - 彼は経験が長いだけに、自信を持って演説ができた。
 *   (As one might expect from his extensive experience, he confidently gave the speech.)
 * - このアパートは東京駅に近いだけに、家賃が高い。
 *   (This apartment is very close to Tokyo station, and as one might expect, the rent is expensive.)
 * - このアプリは便利なだけに、どんどんとユーザーが増えてきています。
 *   (This application is becoming more and more popular just because it is so useful.)
 * - プロ棋士だけに、将棋との向き合い方が半端じゃない。
 *   (As one might expect from a professional shogi player, the way he approaches the game is not careless.)
 * - 夏休みであるだけに、海水浴場には人がたくさんいる。
 *   (It is summer vacation and as one might expect, there are a lot of people at the beach.)
 *
 * Key discriminators:
 * - だけ is the limiting particle (ADP/PART)
 * - に is the adverbial particle (ADP)
 * - Follows verbs, adjectives, or nouns
 * - The だけ+に combination forms a conjunctive pattern
 * - Different from simple だけ (only/just) + に (to/at)
 * - Different from だけでなく (not only... but also)
 * - Different from だけあって (as might be expected - positive evaluation only)
 *
 * GiNZA parse structure:
 * - Various POS tags for だけ (ADP, PART)
 * - に as ADP
 * - Often has case or mark dependencies
 * - May appear as compound or fixed expression
 *
 * Different from:
 * - だけ (dake) - "only/just" without the に conjunction
 * - だけあって (dakeatte) - "as might be expected" (positive evaluation only)
 * - だけでなく (dakedenaku) - "not only... but also"
 * - にしては (nishite) - "considering, for"
 */
export default linguisticRule('だけに', (r) => {
  r.either(
    // Pattern 1: Verb + だけに
    // e.g., 勉強しているだけに, 経験が長いだけに, 合格したと思っていただけに
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const dake = b1.tok({ text: 'だけ' }, 'dake');
      const ni = b1.particle('に', 'ni');

      b1.inOrder(verb, dake, 5);
      b1.inOrder(dake, ni, 1);

      b1.captureSpan('だけに', verb, ni);
    },

    // Pattern 2: I-adjective + だけに
    // e.g., 近いだけに, 長いだけに, 優しいだけに
    (b2) => {
      const adj = b2.adj({}, 'adj');
      const dake = b2.tok({ text: 'だけ' }, 'dake');
      const ni = b2.particle('に', 'ni');

      b2.inOrder(adj, dake, 5);
      b2.inOrder(dake, ni, 1);

      b2.captureSpan('だけに', adj, ni);
    },

    // Pattern 3: Na-adjective + な + だけに
    // e.g., 便利なだけに, 高額なだけに
    (b3) => {
      const naAdj = b3.adj({}, 'naAdj');
      const na = b3.tok({ text: 'な', posOneOf: ['AUX', 'PART'] }, 'na');
      const dake = b3.tok({ text: 'だけ' }, 'dake');
      const ni = b3.particle('に', 'ni');

      b3.inOrder(naAdj, na, 1);
      b3.inOrder(na, dake, 1);
      b3.inOrder(dake, ni, 1);

      b3.captureSpan('だけに', naAdj, ni);
    },

    // Pattern 4: Noun + だけに
    // e.g., プロ棋士だけに, スチュワーデスだけに, 大工だけに
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dake = b4.tok({ text: 'だけ' }, 'dake');
      const ni = b4.particle('に', 'ni');

      b4.inOrder(noun, dake, 1);
      b4.inOrder(dake, ni, 1);

      b4.captureSpan('だけに', noun, ni);
    },

    // Pattern 5: Noun + だ/である + だけに
    // e.g., 夏休みであるだけに, 最新だけに (implied だ)
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = b5.aux({ lemmaOneOf: ['だ', 'である'] }, 'da');
      const dake = b5.tok({ text: 'だけ' }, 'dake');
      const ni = b5.particle('に', 'ni');

      b5.inOrder(noun, da, 3);
      b5.inOrder(da, dake, 3);
      b5.inOrder(dake, ni, 1);

      b5.captureSpan('だけに', noun, ni);
    },

    // Pattern 6: Combined tokenization - だけに as single token
    // Sometimes GiNZA parses だけに as a single token
    (b6) => {
      const verbOrAdj = b6.tok({ posOneOf: ['VERB', 'ADJ'] }, 'verbOrAdj');
      const dakeni = b6.tok({ text: 'だけに' }, 'dakeni');

      b6.inOrder(verbOrAdj, dakeni, 5);

      b6.captureSpan('だけに', verbOrAdj, dakeni);
    },

    // Pattern 7: Noun + single token だけに
    (b7) => {
      const noun = b7.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dakeni = b7.tok({ text: 'だけに' }, 'dakeni');

      b7.inOrder(noun, dakeni, 3);

      b7.captureSpan('だけに', noun, dakeni);
    },

    // Pattern 8: Catch-all - any predicate + だけ + に (loose constraints)
    // Handles unexpected GiNZA tokenizations
    (b8) => {
      const predicate = b8.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON'] }, 'predicate');
      const dake = b8.tok({ textOneOf: ['だけ', 'だけに'] }, 'dake');
      const ni = b8.particle('に', 'ni');

      b8.inOrder(predicate, dake, 5);
      b8.inOrder(dake, ni, 2);

      b8.captureSpan('だけに', predicate, ni);
    }
  );
});
