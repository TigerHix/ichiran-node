import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: しかしながら (shikashinagara) - However, nevertheless
 *
 * A formal conjunction meaning "however" or "nevertheless." This is an
 * emphasized, literary version of しかし (however), formed by combining
 * しかし + ながら (while/although).
 *
 * Structure:
 * - しかし + ながら (fixed expression/conjunction)
 *
 * This conjunction appears at the beginning of a sentence or clause,
 * connecting two contrasting ideas in a formal or literary context.
 * It's more emphatic than しかし alone.
 *
 * Examples:
 * - 日本は安全な国だと言われている。しかしながら１００％安全というわけでもない。
 *   (Japan is said to be a safe country. However, it is not 100% safe.)
 * - 精一杯頑張れば夢が叶うと言われている。しかしながら、人生はそう甘くない。
 *   (It is said that if you work as hard as you can, your dreams will come true. However, life is not so easy.)
 * - そのアイデアはいいと思います。しかしがら、我々の予算だとそのプランを実行することはできないでしょう。
 *   (I think the idea is good. However, our budget will not allow us to implement that plan.)
 *
 * Key discriminators:
 * - しかし is the conjunction (CCONJ) with dep=cc or dep=dep
 * - ながら is a particle/suffix (PART/SCONJ) with dep=fixed
 *
 * GiNZA parse structure:
 * - しかし(CCONJ/ADV,dep=cc/dep) + ながら(PART/SCONJ,dep=fixed)
 * - Sometimes parsed as a single token in some contexts
 *
 * Different from:
 * - しかし alone (less formal)
 * - しかし + ながら as separate grammatical components (verb form)
 * - ながら as "while" used with verb stems (働きながら)
 */
export default linguisticRule('しかしながら', (r) => {
  r.either(
    // Pattern 1: しかし(CCONJ) + ながら(PART/SCONJ) with fixed dependency
    // Standard parsing: しかし as coordinating conjunction
    (b1) => {
      const shikashi = b1.tok({
        text: 'しかし',
        posOneOf: ['CCONJ', 'ADV'],
        depOneOf: ['cc', 'dep'],
      }, 'shikashi');

      const nagara = b1.tok({
        text: 'ながら',
        posOneOf: ['PART', 'SCONJ'],
        dep: 'fixed',
      }, 'nagara');

      b1.inOrder(shikashi, nagara, 1);
      b1.captureSpan('しかしながら', shikashi, nagara);
    },

    // Pattern 2: しかし + ながら with looser dependencies
    // For sentences where GiNZA assigns different dep labels
    (b2) => {
      const shikashi = b2.tok({
        text: 'しかし',
        depOneOf: ['cc', 'dep', 'discourse'],
      }, 'shikashi');

      const nagara = b2.tok({
        text: 'ながら',
        dep: 'fixed',
      }, 'nagara');

      b2.inOrder(shikashi, nagara, 1);
      b2.captureSpan('しかしながら', shikashi, nagara);
    },

    // Pattern 3: しかし + として (catch-all for alternative parsings)
    // Sometimes ながら may be analyzed with different POS tags
    (b3) => {
      const shikashi = b3.tok({
        text: 'しかし',
      }, 'shikashi');

      const nagara = b3.tok({
        textOneOf: ['ながら', '乍ら'],
      }, 'nagara');

      b3.inOrder(shikashi, nagara, 1);
      b3.captureSpan('しかしながら', shikashi, nagara);
    }
  );
});
