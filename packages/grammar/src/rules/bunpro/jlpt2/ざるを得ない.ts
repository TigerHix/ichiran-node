import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ざるを得ない (zaru o enai) - Have no choice but to, Cannot help but
 *
 * A formal, old-fashioned double-negative structure indicating that one cannot avoid doing (A).
 * Literally means "cannot not do (A)" - expressing that an action is unavoidable due to
 * external circumstances or necessity.
 *
 * Structures:
 * - Verb［stem］+ ざるを得ない (casual)
 * - Verb［stem］+ ざるを得ません (polite)
 * - Verb［stem］+ ざるを得なかった (past)
 * - Verb［stem］+ ざるを得ませんでした (past polite)
 *
 * Special conjugation for する:
 * - する → せざるを得ない (NOT しざるを得ない)
 *
 * Special conjugation for 来る (kuru):
 * - 来る → こざるを得ない (NOT きざるを得ない)
 *
 * Examples:
 * - 出勤時間を変更せざるを得ない状況です。
 *   (I have no choice but to change my commute time.)
 * - 新しいのをかわざるを得ない。
 *   (I have no choice but to buy a new one.)
 * - 諦めざるを得ない。
 *   (I have no choice but to give up.)
 * - 従わざるをえません。
 *   (I have no choice but to obey.)
 * - 感動せざるをえなかった。
 *   (I couldn't help but be moved.)
 *
 * Key discriminators:
 * - ざる is the old-fashioned negative form (attributive form of ざり)
 * - Connects to verb stem (masu stem/連用形)
 * - Different from ざる alone (classical negative form modifying nouns)
 * - Different from 得ない alone (objective impossibility)
 * - Formal register, used in writing or formal speech
 * - Double-negative structure expressing unavoidable necessity
 *
 * GiNZA parse structure:
 * - ざる may be parsed as AUX or as part of verb conjugation
 * - を is ADP (particle) with dep=case
 * - 得ない is typically AUX with dep=aux
 *
 * Important: Must match verb stem + ざる + を + 得ない pattern
 * to exclude:
 * - ざる alone (classical negative attributive form)
 * - 得ない alone (cannot, unable to)
 * - Verb + ない (simple negation)
 */
export default linguisticRule('ざるを得ない', (r) => {
  r.either(
    // Branch 1: ざる as AUX attached to verb stem + を + 得ない
    (b) => {
      const zaru = b.aux({
        lemma: 'ざる',
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enai = b.aux({
        lemma: '得ない',
      }, 'enai');
      b.inOrder(zaru, wo, 1);
      b.inOrder(wo, enai, 1);
      b.captureSpan('ざるを得ない', zaru, enai);
    },

    // Branch 2: ざる as AUX with text constraint + を + 得ない
    // Some parses may have different POS tagging
    (b) => {
      const zaru = b.tok({
        text: 'ざる',
        posOneOf: ['AUX', 'VERB'],
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enai = b.tok({
        lemmaOneOf: ['得ない', 'える'],
        posOneOf: ['AUX', 'VERB'],
      }, 'enai');
      b.inOrder(zaru, wo, 1);
      b.inOrder(wo, enai, 1);
      b.captureSpan('ざるを得ない', zaru, enai);
    },

    // Branch 3: ざる + を + 得ない with explicit text matching
    // This catches cases where POS tagging is inconsistent
    (b) => {
      const zaru = b.tok({
        text: 'ざる',
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enai = b.tok({
        textOneOf: ['得ない', '得ません', '得なかった', '得ませんでした'],
      }, 'enai');
      b.inOrder(zaru, wo, 1);
      b.inOrder(wo, enai, 1);
      b.captureSpan('ざるを得ない', zaru, enai);
    }
  );
});
