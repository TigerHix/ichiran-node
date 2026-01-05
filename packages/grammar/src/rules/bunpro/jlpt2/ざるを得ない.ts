import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ざるを得ない (zaru o enai) - Have no choice but to, Can't help but
 *
 * A formal, old-fashioned structure expressing that one has no alternative
 * but to do something. It's a double negative meaning "cannot not do".
 * This is the literary form of ないわけにはいかない.
 *
 * Structures:
 * - Verb［stem (mizenkei)］+ ざる + を + 得ない (casual/literary)
 * - Verb［stem (mizenkei)］+ ざる + を + 得ません (polite)
 * - Past: ざるを得なかった, ざるを得ませんでした
 *
 * Verb conjugation:
 * - 五段 verbs: 買う → 買わざるを得ない (kau → kawazaru o enai)
 * - サ変 verbs: する → せざるを得ない (suru → sezaru o enai)
 * - 一段 verbs: 見る → 見ざるを得ない (miru → mizaru o enai)
 * - カ変 verbs: 来る → こざるを得ない (kuru → kozaru o enai)
 *
 * Examples:
 * - 環境保護のために、工場の排気量を規制せざるを得ない。
 *   (We have no choice but to regulate factory emissions for environmental protection.)
 * - 新しいのをかわざるを得ない。
 *   (I have no choice but to buy a new one.)
 * - 先輩に指示されたので従わざるを得ません。
 *   (Since my senpai instructed me to do it, I have no choice but to obey.)
 * - 出勤時間を変更せざるを得ない状況です。
 *   (It's a situation where I have no choice but to change my commute time.)
 *
 * Key discriminators:
 * - ざる is an auxiliary verb (lemma=ぬ) in attributive form (連体形-補助)
 * - Always attached to verb mizenkei (未然形) stem
 * - Fixed expression: ざる + を + 得 + auxiliary (ない/ません)
 * - All parts have dep=fixed indicating this is a fixed phrase
 * - The "を" particle in this expression has dep=fixed (not dep=case like object marker)
 * - The "得" verb is in mizenkei (未然形-一般) with lemma=得る
 * - Different from を得る (wo eru - to gain) where を is object marker
 * - Different from ざる alone (classical negative form modifying nouns)
 *
 * GiNZA parse structure:
 * - Verb (mizenkei) + ざる(AUX, lemma=ぬ, dep=aux, inflectionForm=連体形-補助)
 * - を(ADP, dep=fixed) + 得(VERB, lemma=得る, dep=fixed, inflectionForm=未然形-一般)
 * - ない/ます(AUX, dep=fixed)
 *
 * Important: Matches only the complete ざるを得(え)ない/ません pattern
 * to exclude:
 * - ざる alone (classical negative attributive form)
 * - を得る (wo eru - to gain/acquire, different grammar)
 * - Simple negative forms without the fixed expression structure
 */
export default bunproLinguisticRule('ざるを得ない', (r) => {
  // The pattern is: Verb(mizenkei) + ざる + を + 得/え + ない/ません
  // This grammar has two different parse structures in GiNZA:
  // 1. Casual form (得ない/えない): All tokens have dep=fixed, 得/え is in mizenkei
  // 2. Polite form (得ません/えません): Different structure, 得/え is in ren'youkei

  r.either(
    // Branch 1: Casual form (得ない/えません)
    // GiNZA parses this as a fixed expression with dep=fixed
    (b) => {
      // ざる auxiliary (classical negative form, lemma=ぬ)
      const zaru = b.aux({
        lemma: 'ぬ',
        text: 'ざる',
        inflectionForm: '連体形-補助',
      }, 'zaru');

      // Particle を with dep=fixed (part of the fixed expression)
      const wo = b.particle('を', 'wo');

      // 得/え verb (lemma=得る or える) in mizenkei form
      const eru = b.verb({
        lemmaOneOf: ['得る', 'える'],
        inflectionForm: '未然形-一般',
      }, 'eru');

      // Final auxiliary (ない for casual)
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');

      // Require the fixed structure: ざる + を + 得 + ない
      b.headChild(zaru, wo, 'fixed');
      b.headChild(zaru, eru, 'fixed');
      b.headChild(zaru, nai, 'fixed');

      // Require sequential order
      b.inOrder(zaru, wo, 1);
      b.inOrder(wo, eru, 1);
      b.inOrder(eru, nai, 1);

      // Capture the entire span from ざる to ない
      b.captureSpan('ざるを得ない', zaru, nai);
    },

    // Branch 2: Polite form (得ません/えません)
    // GiNZA parses this differently: 得/え is in ren'youkei and is the root
    (b) => {
      // ざる auxiliary (classical negative form, lemma=ぬ)
      const zaru = b.aux({
        lemma: 'ぬ',
        text: 'ざる',
        inflectionForm: '連体形-補助',
      }, 'zaru');

      // Particle を (can be dep=case or dep=fixed depending on parse)
      const wo = b.tok({
        text: 'を',
        pos: 'ADP',
      }, 'wo');

      // 得/え verb (lemma=得る or える) in ren'youkei form
      // This is the root of the clause
      const eru = b.verb({
        lemmaOneOf: ['得る', 'える'],
        inflectionForm: '連用形-一般',
      }, 'eru');

      // Negative polite auxiliary (ません)
      // The polite negative is formed as: 得(ren'youkei) + ませ(mizenkei of ます) + ん
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');

      // Final ん (from ません, lemma=ぬ)
      const n = b.aux({
        lemma: 'ぬ',
        text: 'ん',
      }, 'n');

      // Require sequential order: ざる + を + 得 + ませ + ん
      b.inOrder(zaru, wo, 1);
      b.inOrder(wo, eru, 1);
      b.inOrder(eru, mase, 1);
      b.inOrder(mase, n, 1);

      // Capture the entire span from ざる to ん
      b.captureSpan('ざるを得ない', zaru, n);
    }
  );
});
