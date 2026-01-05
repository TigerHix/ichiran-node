import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ざる (zaru) - Classical negative form (attributive)
 *
 * An archaic/literary auxiliary verb expressing negation, equivalent to modern
 * ない in its attributive form. It's the classical negative form of verbs,
 * used primarily in fixed expressions and literary contexts.
 *
 * Structure:
 * - Verb［mizenkei/irrealis stem］+ ざる
 *
 * Examples:
 * - 知られざる傑作 (an unknown masterpiece)
 * - たゆまざる努力 (unwavering effort)
 * - 絶えざる失敗 (unending failures)
 * - 消えざる傷 (a wound that will not disappear)
 *
 * Key discriminators:
 * - ざる is the attributive form (連体形) of the classical auxiliary ず/ぬ
 * - Attaches to verb mizenkei (未然形 - irrealis stem)
 * - GiNZA parses it as AUX with lemma=ぬ or lemma=ず
 * - inflectionForm: 連体形-補助 (attributive form)
 * - conjugationClass: 助動詞-ヌ or 文語助動詞-ズ
 * - dep: aux (attached to verb)
 * - Must modify a noun (used pre-nominally)
 *
 * Different from:
 * - ず (zu) - Terminal form of same auxiliary (used to end sentences)
 * - ぬ (nu) - Another form of the classical negative
 * - ざるを得ない (zaru o enai) - Related grammar meaning "have no choice but to"
 *
 * GiNZA parse structure:
 * - Verb (mizenkei) + ざる(AUX, lemma=ぬ/ず, dep=aux)
 * - ざる has inflectionForm=連体形-補助
 * - ざる modifies the following noun via acl dependency
 */
export default linguisticRule('ざる', (r) => {
  r.either(
    // Branch 1: ざる with lemma=ぬ, classical auxiliary ヌ
    (b1) => {
      const zaru = b1.aux({
        text: 'ざる',
        lemma: 'ぬ',
        conjugationClass: '助動詞-ヌ',
        dep: 'aux',
      }, 'zaru');
      b1.capture(zaru);
    },

    // Branch 2: ざる with lemma=ず, classical auxiliary ズ
    (b2) => {
      const zaru = b2.aux({
        text: 'ざる',
        lemma: 'ず',
        conjugationClass: '文語助動詞-ズ',
        dep: 'aux',
      }, 'zaru');
      b2.capture(zaru);
    },

    // Branch 3: ざる with inflectionForm constraint (catch-all for both lemma types)
    // This handles any ざる in attributive form functioning as auxiliary
    (b3) => {
      const zaru = b3.aux({
        text: 'ざる',
        inflectionForm: '連体形-補助',
        dep: 'aux',
      }, 'zaru');
      b3.capture(zaru);
    }
  );
});
