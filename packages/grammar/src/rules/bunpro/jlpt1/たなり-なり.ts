import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('たなり-なり', (r) => {
  // Match verb ta-form + なり pattern
  // Pattern: Verb[ta-form] + た/だ + なり (e.g., 行ったなり, 入ったなり, 倒れ込んだなり)
  // Means: "did X and remained in that state" or "ever since X happened"

  r.either(
    // Pattern 1: Standard verb + た + なり (e.g., 行ったなり, 入ったなり, 腰かけたなり)
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const ta = b1.aux({ lemma: 'た' }, 'ta');
      const nari = b1.tok({ text: 'なり' }, 'nari');
      b1.auxOf(verb, ta);
      b1.inOrder(ta, nari, 2);
      b1.captureSpan('たなり-なり', verb, nari);
    },

    // Pattern 2: Verb stem (ADJ) + だ + なり (e.g., 倒れ込んだなり)
    // GiNZA sometimes parses the verb stem as ADJ with copula だ
    (b2) => {
      const verb = b2.tok({ posOneOf: ['VERB', 'ADJ'] }, 'verb');
      const da = b2.aux({ lemma: 'だ' }, 'da');
      const nari = b2.tok({ text: 'なり' }, 'nari');
      b2.auxOf(verb, da);
      b2.inOrder(da, nari, 2);
      b2.captureSpan('たなり-なり', verb, nari);
    }
  );
});
