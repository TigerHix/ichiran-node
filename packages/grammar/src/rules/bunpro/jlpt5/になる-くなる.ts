import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('になる-くなる', (r) => {
  // になる/くなる: becomes (change of state)
  // Patterns:
  // - Noun + に + なる (noun becomes)
  // - Na-adj + に + なる (na-adjective becomes)
  // - I-adj stem + なる (i-adjective becomes, e.g. おいしくなる)

  r.either(
    // Pattern 1: Noun/Na-adj + に + なる
    // Both nouns and na-adjectives use the same structure with particle に or auxiliary に
    (b) => {
      // Noun, na-adjective, or adverb (GiNZA sometimes parses i-adj stem as ADV)
      const base = b.tok({
        posOneOf: ['NOUN', 'ADJ', 'ADV'],
        // Exclude i-adj stems with conjugationClass=形容詞 (those are pattern 2)
      }, 'base');

      // Particle に (for nouns) or auxiliary に from copula だ (for na-adj)
      const ni = b.tok({ text: 'に', posOneOf: ['ADP', 'AUX'] }, 'ni');

      // Verb なる (become)
      const naru = b.verb({ lemma: 'なる' }, 'naru');

      b.inOrder(base, ni, 1);
      b.inOrder(ni, naru, 1);
      b.captureSpan('になる-くなる', base, naru);
    },

    // Pattern 2: I-adjective stem + なる (any form)
    // e.g. おいしくなる, むずかしくなる, おいしくなった, おいしくなります
    // GiNZA parses i-adj stems with various POS (ADJ, VERB, NOUN, ADV) but always with conjugationClass=形容詞
    (b) => {
      // I-adjective stem in 連用形-一般 (ku-form)
      // Match by conjugationClass since POS varies (ADJ, VERB, NOUN, ADV)
      const adj = b.tok({
        conjugationClass: '形容詞',
        inflectionForm: '連用形-一般',
      }, 'adj');

      // Verb なる (become) - can be in various forms (なる, なった, なります, etc.)
      const naru = b.verb({ lemma: 'なる' }, 'naru');

      b.inOrder(adj, naru, 1);
      b.captureSpan('になる-くなる', adj, naru);
    }
  );
});
