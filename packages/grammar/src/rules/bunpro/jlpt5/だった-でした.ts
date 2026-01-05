import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: だった・でした (was, were - past copula)
 *
 * Past tense copula for nouns and na-adjectives.
 * - Casual: Noun/na-adj + だった
 * - Polite: Noun/na-adj + でした
 *
 * Examples:
 * - 綺麗だった (was beautiful - casual)
 * - 静かでした (was quiet - polite)
 * - トムだった (was Tom - casual)
 * - それは昨日でした (that was yesterday - polite)
 *
 * Note: GiNZA uses dep='cop' for nouns, but dep='aux' for na-adjectives.
 *
 * GiNZA parse structure:
 * - Casual: noun/na-adj (root) <- だっ (cop/aux) <- た (aux, head points to root)
 * - Polite: noun/na-adj (root) <- でし (cop/aux) <- た (aux, head points to root)
 */
export default bunproLinguisticRule('だった-でした', (r) => {
  r.either(
    // Pattern 1: Casual - だった (past of だ)
    // Structure: noun/na-adj + だっ + た (both auxiliaries attach to head)
    (branch) => {
      branch.either(
        // Noun + だった (dep='cop' for だっ)
        (b1) => {
          const head = b1.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
          const dat = b1.aux({ lemma: 'だ', dep: 'cop' }, 'dat');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.copulaOf(head, dat);
          b1.auxOf(head, ta);
          b1.inOrder(dat, ta, 1);
          b1.captureSpan('だった', dat, ta);
        },
        // Na-adjective + だった (dep='aux' for だっ)
        (b1) => {
          const naAdj = b1.adj({}, 'naAdj');
          const dat = b1.aux({ lemma: 'だ', dep: 'aux' }, 'dat');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.auxOf(naAdj, dat);
          b1.auxOf(naAdj, ta);
          b1.inOrder(dat, ta, 1);

          // The naAdj must not be an i-adjective
          b1.not((nr) => {
            nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
          });

          b1.captureSpan('だった', dat, ta);
        }
      );
    },
    // Pattern 2: Polite - でした (past of です)
    // Structure: noun/na-adj + でし + た (both auxiliaries attach to head)
    (branch) => {
      branch.either(
        // Noun + でした (dep='cop' for でし)
        (b1) => {
          const head = b1.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
          const deshi = b1.aux({ lemma: 'です', dep: 'cop' }, 'deshi');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.copulaOf(head, deshi);
          b1.auxOf(head, ta);
          b1.inOrder(deshi, ta, 1);
          b1.captureSpan('でした', deshi, ta);
        },
        // Na-adjective + でした (dep='aux' for でし)
        // Also matches i-adjectives + でした (politeness marker, not true copula)
        (b1) => {
          const adj = b1.adj({}, 'adj');
          const deshi = b1.aux({ lemma: 'です', dep: 'aux' }, 'deshi');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.auxOf(adj, deshi);
          b1.auxOf(adj, ta);
          b1.inOrder(deshi, ta, 1);
          b1.captureSpan('でした', deshi, ta);
        }
      );
    }
  );
});
