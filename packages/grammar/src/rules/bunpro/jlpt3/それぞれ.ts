import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('それぞれ', (r) => {
  // それぞれ - adverb/noun meaning "each, respectively, individual"
  // Indicates separate/different treatment of items in a group
  // Patterns:
  // - それぞれ + verb: それぞれ食べる, それぞれ行く, それぞれ違う
  // - それぞれ + i-adjective: それぞれ好き, それぞれ違う
  // - それぞれの + noun: それぞれのチーム, それぞれの道, それぞれの意見
  // - それぞれ + noun (some cases): それぞれ別の行動

  // Note: GiNZA tags それぞれ as:
  // - ADV when used as adverb: それぞれ食べる, それぞれ好き
  // - NOUN or PRON when used with の: それぞれのチーム

  const sorezore = r.tok({
    lemma: 'それぞれ',
    posOneOf: ['ADV', 'NOUN', 'PRON'],
  }, 'sorezore');

  r.either(
    // Pattern 1: それぞれの + noun
    // それぞれのチーム, それぞれの道, それぞれの意見, それぞれの役割
    // それぞれの原因, それぞれの考え方
    (b) => {
      const no = b.particle('の', 'no');
      const noun = b.noun({}, 'noun');
      b.inOrder(sorezore, no, 1);
      b.inOrder(no, noun, 2);
      b.captureSpan('それぞれ', sorezore, noun);
    },

    // Pattern 2: それぞれ + verb
    // それぞれ食べる, それぞれ行く, それぞれ違う, それぞれ工夫する
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(sorezore, verb, 3);
      b.captureSpan('それぞれ', sorezore, verb);
    },

    // Pattern 3: それぞれ + i-adjective
    // それぞれ好き, それぞれ違う
    (b) => {
      const adj = b.adj({
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'adj');
      b.inOrder(sorezore, adj, 2);
      b.captureSpan('それぞれ', sorezore, adj);
    },

    // Pattern 4: それぞれ + noun (direct, less common)
    // それぞれ別の行動
    (b) => {
      const noun = b.noun({}, 'noun');
      b.inOrder(sorezore, noun, 2);
      b.captureSpan('それぞれ', sorezore, noun);
    }
  );
});
