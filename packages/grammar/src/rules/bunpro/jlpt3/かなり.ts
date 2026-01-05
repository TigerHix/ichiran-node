import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('かなり', (r) => {
  // かなり - adverb meaning "quite, considerably, pretty"
  // Patterns:
  // - かなり + i-adjective: かなり美味しい, かなり痛い, かなり重い
  // - かなり + verb: かなり疲れた, かなり節約した
  // - かなりの + noun: かなりの距離, かなりの人, かなりの確率
  // - かなり + noun (direct): かなり遠くの方, かなり上級レベル

  // Note: GiNZA tags かなり as:
  // - ADV when used as adverb: かなり美味しい, かなり疲れた
  // - ADJ (形状詞-一般) when followed by の: かなりの距離

  const kanari = r.tok({
    lemma: 'かなり',
    posOneOf: ['ADV', 'ADJ'],
  }, 'kanari');

  r.either(
    // Pattern 1: かなり + i-adjective
    // かなり美味しい, かなり痛い, かなり重い, かなり遠い
    (b) => {
      const adj = b.adj({
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'adj');
      b.inOrder(kanari, adj, 2);
      b.captureSpan('かなり', kanari, adj);
    },

    // Pattern 2: かなり + na-adjective
    // かなり厳しい人 (kanari + na-adj + noun)
    (b) => {
      const adj = b.adj({
        tag: '形状詞-一般',
      }, 'adj');
      b.inOrder(kanari, adj, 2);
      b.captureSpan('かなり', kanari, adj);
    },

    // Pattern 3: かなりの + noun
    // かなりの距離, かなりの人, かなりの確率, かなりの腕前
    (b) => {
      const no = b.particle('の', 'no');
      const noun = b.noun({}, 'noun');
      b.inOrder(kanari, no, 1);
      b.inOrder(no, noun, 2);
      b.captureSpan('かなり', kanari, noun);
    },

    // Pattern 4: かなり + noun (direct modification, adverbial nouns)
    // かなり遠くの方, かなり上級レベル
    (b) => {
      const noun = b.noun({}, 'noun');
      b.inOrder(kanari, noun, 2);
      b.captureSpan('かなり', kanari, noun);
    },

    // Pattern 5: かなり + verb (affirmative)
    // かなり疲れた, かなり節約した, かなり使った, かなり異なる
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(kanari, verb, 3);
      b.captureSpan('かなり', kanari, verb);
    }
  );
});
