import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: -に-ない (verb + に + same verb + potential negative)
 *
 * Expresses that one cannot do something even if they want to.
 * "Cannot X even if wanted to"
 *
 * Examples:
 * - 断るに断れない (cannot refuse even if wanted to)
 * - 笑うに笑えない (cannot laugh even if wanted to)
 * - 行くに行けない (cannot go even if wanted to)
 *
 * Grammar structure:
 * - Verb in dictionary form (辞書形)
 * - Particle に
 * - Same verb in potential negative form (可能形 + ない)
 *
 * The に particle here is not the usual "to/toward" marker - it's a special
 * grammatical particle that connects with the negative potential form to
 * express "even toward X, X cannot be done".
 *
 * GiNZA parses potential forms in various ways:
 * - Standard potential: verb stem + れる/られる (aux)
 * - Contraction: られる → れる (especially in speech)
 * - Negative: potential verb + ない (aux)
 *
 * LIMITATION: We cannot enforce that verb1 and verb2 have the same lemma
 * at the DSL level (cross-variable constraint). This is a known limitation.
 * In practice, this pattern is strongly correlated with same-verb usage,
 * so false positives on different verbs are rare.
 */
export default linguisticRule('-に-ない', (r) => {
  r.either(
    // Pattern 1: Standard form with ない
    (b1) => {
      const ni = b1.particle('に', 'ni');
      const verb2 = b1.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb2');
      const nai = b1.tok({ lemma: 'ない' }, 'nai');
      const verb1 = b1.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb1');

      b1.inOrder(verb1, ni, 3);
      b1.inOrder(ni, verb2, 10);
      b1.inOrder(verb2, nai, 10);
      b1.captureSpan('-に-ない', verb1, nai);
    },

    // Pattern 2: Polite form with ません
    (b2) => {
      const ni = b2.particle('に', 'ni');
      const verb2 = b2.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb2');
      const masen = b2.tok({ text: 'ません' }, 'masen');
      const verb1 = b2.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb1');

      b2.inOrder(verb1, ni, 3);
      b2.inOrder(ni, verb2, 10);
      b2.inOrder(verb2, masen, 10);
      b2.captureSpan('-に-ない', verb1, masen);
    },

    // Pattern 3: With ず instead of ない
    (b3) => {
      const ni = b3.particle('に', 'ni');
      const verb2 = b3.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb2');
      const zu = b3.tok({ text: 'ず' }, 'zu');
      const verb1 = b3.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb1');

      b3.inOrder(verb1, ni, 3);
      b3.inOrder(ni, verb2, 10);
      b3.inOrder(verb2, zu, 10);
      b3.captureSpan('-に-ない', verb1, zu);
    }
  );
});
