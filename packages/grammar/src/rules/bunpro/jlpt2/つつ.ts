import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: つつ (tsutsu) - While doing, Although (formal/literary)
 *
 * A formal conjunction particle expressing simultaneous actions or ongoing states.
 * Indicates that (A) and (B) are ongoing actions happening at the same time.
 * More formal and literary than ながら (nagara), typically used in writing.
 *
 * Structure:
 * - Verb［stem/masu form minus ます］+ つつ
 *
 * Examples:
 * - 環境に悪いと知りつつ、レジ袋を使い続けている。
 *   (While knowing it's bad for the environment, I continue to use plastic bags.)
 * - インターネットでレシピを見つつ、料理をした。
 *   (I cooked while looking at a recipe on the internet.)
 * - 仕事に関係ないことを考えつつ仕事をしていたら、大きなミスをしてしまった。
 *   (I made a big mistake while working while thinking about something unrelated to work.)
 *
 * Key discriminators:
 * - つつ is a particle (SCONJ in GiNZA) following verb stem
 * - Verb stem is in 連用形-一般 (masu stem/ren'youkei)
 * - Used for simultaneous ongoing actions (formal register)
 * - Different from ながら (more casual, can be used with physical actions)
 * - つつ is preferred for mental states/ongoing conditions
 * - Different from つつも (concessive: "even while/although")
 *
 * GiNZA parse structure:
 * - 知り(VERB, 連用形-一般) + つつ(SCONJ)
 * - 見(VERB, 連用形-一般) + つつ(SCONJ)
 * - Various dependency relations (advcl, root)
 *
 * Important: Matches verb stem + つつ to exclude:
 * - つつも (concessive form - different grammar point)
 * - Independent use of つつ (rare as standalone)
 * - ながら (more casual simultaneous action marker)
 */
export default bunproLinguisticRule('つつ', (r) => {
  // Verb masu stem + つつ (while doing X)
  // Similar pattern to ながら but more formal/literary
  r.either(
    // Pattern 1: Standard verb stems with inflectionForm=連用形-一般
    // e.g., 知りつつ, 見つつ, しつつ, 考えつつ
    (branch1) => {
      const verbStem = branch1.tok({
        inflectionForm: '連用形-一般',
        posOneOf: ['VERB', 'AUX'],
        depOneOf: ['advcl', 'aux', 'root', 'obj', 'obl', 'acl']
      }, 'verbStem');

      const tsutsu = branch1.tok({ lemma: 'つつ', pos: 'SCONJ' }, 'tsutsu');

      branch1.inOrder(verbStem, tsutsu, 1);
      branch1.captureSpan('つつ', verbStem, tsutsu);
    },

    // Pattern 2: Edge case for hiragana-only verbs without inflectionForm
    // e.g., some GiNZA parses may lack inflectionForm for certain stems
    // Must still have dep=advcl to ensure it's a verb stem in adverbial clause
    (branch2) => {
      const verbStem = branch2.verb({
        dep: 'advcl'
      }, 'verbStem');

      const tsutsu = branch2.tok({ lemma: 'つつ', pos: 'SCONJ' }, 'tsutsu');

      // Exclude verbs that have inflectionForm (those are handled by branch 1)
      branch2.not((n) => {
        n.verb({
          inflectionForm: '連用形-一般'
        }, 'verbStem');
      });

      branch2.inOrder(verbStem, tsutsu, 1);
      branch2.captureSpan('つつ', verbStem, tsutsu);
    }
  );
});
