import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('です', (r) => {
  r.either(
    // Pattern 1: Noun + です (copula)
    // e.g., 学生です, 日本人です, レストランです
    // GiNZA parses as: NOUN (root) <- です (cop)
    (r1) => {
      const noun = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'NUM'] }, 'noun');
      const desu = r1.aux({ text: 'です', dep: 'cop' }, 'desu');
      r1.copulaOf(noun, desu);
      r1.capture(desu);
    },
    // Pattern 2: Adjective + です (politeness marker)
    // Includes both na-adjectives and i-adjectives
    // e.g., 綺麗です, 大好きです, 暑いです, 面白いです
    // GiNZA parses as: ADJ (root) <- です (aux)
    // Note: For adjectives, です is a politeness marker, not a true copula,
    // so GiNZA uses dep='aux' instead of dep='cop'
    (r2) => {
      const adj = r2.adj({}, 'adj');
      const desu = r2.aux({ text: 'です', dep: 'aux' }, 'desu');
      r2.auxOf(adj, desu);
      r2.capture(desu);
    }
  );
});
