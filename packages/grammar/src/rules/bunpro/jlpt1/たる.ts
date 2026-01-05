import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('たる', (r) => {
  // たる - classical/formal copula (attributive form of たり)
  // Meaning: "as X", "being X" (formal/literary)
  // Patterns:
  //   - noun + たる + noun (e.g., 教師たる者)
  //   - noun + たる + に (e.g., 社員たるに相応しい)
  //   - noun + たる (followed by adjective/verb phrase)
  //
  // GiNZA parses たる as:
  //   - AUX with lemma=たり (classical copula attributive form)
  //   - Sometimes merged with following particles (たるに)

  // Use either() to handle different parsing patterns
  r.either(
    (b) => {
      // Pattern 1: たる as separate token
      const taru = b.tok({
        text: 'たる',
      }, 'taru');

      // Preceded by a noun/pronoun - compound nouns may have multiple tokens
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');

      // Allow compound nouns with multiple parts (e.g., 世界王者 -> 世界+王者)
      b.inOrder(noun, taru, 5);

      b.captureSpan('たる', noun, taru);
    },
    (b) => {
      // Pattern 2: たるに as single token
      const taruni = b.tok({
        text: 'たるに',
      }, 'taru');

      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');

      b.inOrder(noun, taruni, 5);
      b.captureSpan('たる', noun, taruni);
    }
  );
});
