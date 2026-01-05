import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: たまえ (tamoe) - Formal command form
 *
 * An archaic/formal imperative auxiliary verb used to give commands.
 * Originally from the honorific verb 給う (tamau), the imperative form たまえ
 * attaches to verb stems (masu-stem/連用形) to create a formal, authoritative command.
 *
 * Used primarily by men toward those of lower status (boss to subordinate, etc.).
 * More formal and authoritative than standard imperatives like 命令形 or なさい.
 *
 * Structures:
 * - Verb［stem/masu form］+ たまえ
 *
 * Examples:
 * - 勉強をやりたまえ。
 *   (Do your studies! / Study!)
 * - 座りたまえ。
 *   (Please sit down.)
 * - 入りたまえ。
 *   (Come in!)
 * - 出て行きたまえ！
 *   (Get out!)
 * - 許したまえ。
 *   (Forgive us.)
 *
 * Key discriminators:
 * - たまえ is an auxiliary verb/suffix attached to verb stems
 * - Creates a formal, authoritative command
 * - Different from independent use of 給う (to give/bestow)
 * - Different from standard imperatives like 食べろ, 食べなさい
 *
 * GiNZA parse structure (INCONSISTENT):
 * 1. Simple forms: やりたまえ → やり(VERB,連用形) + たまえ(NOUN,lemma=たまう,命令形)
 * 2. With kureru: くれたまえ → くれ(VERB,連用形) + た(AUX) + まえ(NOUN,lemma=まえ)
 * 3. Can be NOUN or AUX depending on context
 *
 * Important: The kureru+たまえ pattern is parsed inconsistently by GiNZA.
 * The "た+まえ" split makes it indistinguishable from "before" (まえ).
 * We match the consistent simple patterns and skip the kureru compounds.
 */
export default bunproLinguisticRule('たまえ', (r) => {
  r.either(
    // Branch 1: たまえ as NOUN with lemma=たまう (standard imperative form)
    // Most consistent parse for simple verb stem + たまえ
    (b) => {
      const tamoe = b.tok({
        textOneOf: ['たまえ', '給え'],
        lemma: 'たまう',
        pos: 'NOUN',
        inflectionForm: '命令形',
      }, 'tamoe');
      b.capture(tamoe);
    },

    // Branch 2: たまえ as AUX attached to verb stem with dep=aux
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const tamoe = b.aux({
        textOneOf: ['たまえ', '給え'],
        dep: 'aux',
      }, 'tamoe');
      b.auxOf(stem, tamoe);
      b.captureSpan('たまえ', stem, tamoe);
    },

    // Branch 3: たまえ as NOUN/VERB following verb stem
    // Catches cases where dependency relation is different
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const tamoe = b.tok({
        textOneOf: ['たまえ', '給え'],
        posOneOf: ['NOUN', 'VERB', 'AUX'],
      }, 'tamoe');
      b.inOrder(stem, tamoe, 3);
      b.captureSpan('たまえ', stem, tamoe);
    }
  );
});
