import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ざるを得ない (zaru o enai) - "have no choice but to, cannot help but, forced to"
 *
 * A formal, archaic/literary construction expressing that one has no choice but to do something.
 * This is a double-negative structure: "not doing (A) cannot be gained" = "must do (A)".
 *
 * ざる is the attributive form (連体形) of the classical auxiliary verb ざり (old negative form),
 * which is equivalent to modern ない. It attaches to the verb negative stem (未然形/mizenkei).
 *
 * Structures:
 * - Verb［negative stem］+ ざる + を + 得ない (casual)
 * - Verb［negative stem］+ ざる + を + 得ません (polite)
 *
 * Formation:
 * - Remove ない from the negative form and attach ざるを得ない
 * - Five-grade verbs (五段動詞): 読まない → 読まざるを得ない
 * - Ichidan verbs (一段動詞): 食べない → 食べざるを得ない
 * - Irregular verbs:
 *   - する → せざるを得ない (NOT しざるを得ない)
 *   - くる → こざるを得ない
 *
 * Examples:
 * - 同意せざるを得ない。
 *   (I have no choice but to agree.)
 * - 認めざるを得ない事実だ。
 *   (It's a fact that cannot be denied / must be acknowledged.)
 * - 出勤時間を変更せざるを得ない状況です。
 *   (We're in a situation where we have no choice but to change work hours.)
 * - 新しいのを買わざるを得ない。
 *   (I have no choice but to buy a new one.)
 * - 古い家屋は倒壊する恐れがあるから、壊さざるを得ません。
 *   (The old building is at risk of collapsing, so we have no choice but to tear it down.)
 * - 今度の上司は外部からの出向だから、気を遣わざるを得ない。
 *   (This superior is being transferred from outside, so I have no choice but to be mindful.)
 * - 諦めざるを得ない。
 *   (I have no choice but to give up.)
 * - ことわらざるを得なかった。
 *   (I had no choice but to decline.)
 *
 * Key discriminators:
 * - ざる is the classical negative form (attributive form of ざり)
 * - More formal and literary than ないわけにはいかない or しかない
 * - Expresses external compulsion or lack of options
 * - Different from ずにはいられない (more subjective/emotional)
 * - Different from てたまらない (emotional compulsion)
 * - Different from てしょうがない (emotional state)
 *
 * GiNZA parse structure:
 * - ざる(AUX) + を(ADP/Case) + 得ない(VERB/AUX)
 * - Various dependency relations (fixed, compound, advcl)
 * - 得ない may be parsed as separate AUX or as part of the verb
 *
 * Important: Matches the full sequence ざるを得ない to avoid partial matches
 * that could confuse with related grammar patterns.
 */
export default linguisticRule('ざるを得ない', (r) => {
  r.either(
    // Branch 1: ざるを得ない as a fixed multi-word sequence
    // Most reliable pattern - matches the full phrase
    (b) => {
      const zaru = b.aux({
        text: 'ざる',
        dep: 'fixed',
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enai = b.aux({
        textOneOf: ['得ない', '得ません'],
        dep: 'fixed',
      }, 'enai');
      b.inOrder(zaru, wo, enai, 5);
      b.captureSpan('ざるを得ない', zaru, enai);
    },

    // Branch 2: ざるを得ない with compound dependency
    // Alternative parsing where parts are linked as compound
    (b) => {
      const zaru = b.aux({
        text: 'ざる',
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enai = b.aux({
        textOneOf: ['得ない', '得ません'],
        dep: 'compound',
      }, 'enai');
      b.inOrder(zaru, wo, enai, 5);
      b.captureSpan('ざるを得ない', zaru, enai);
    },

    // Branch 3: ざるを得ない with advcl dependency
    // ざる modifies the whole construction
    (b) => {
      const zaru = b.aux({
        text: 'ざる',
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enai = b.aux({
        textOneOf: ['得ない', '得ません'],
        dep: 'advcl',
      }, 'enai');
      b.inOrder(zaru, wo, enai, 5);
      b.captureSpan('ざるを得ない', zaru, enai);
    },

    // Branch 4: ざる followed by を and 得ない/得ません
    // Most flexible pattern - just requires them to appear in order
    (b) => {
      const zaru = b.aux({
        text: 'ざる',
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enai = b.tok({
        textOneOf: ['得ない', '得ません'],
      }, 'enai');
      b.inOrder(zaru, wo, enai, 5);
      b.captureSpan('ざるを得ない', zaru, enai);
    },

    // Branch 5: ざるを得ない with past tense
    // ざるを得なかった (past form)
    (b) => {
      const zaru = b.aux({
        text: 'ざる',
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enakatta = b.aux({
        textOneOf: ['得なかった', '得ませんでした'],
      }, 'enakatta');
      b.inOrder(zaru, wo, enakatta, 5);
      b.captureSpan('ざるを得ない', zaru, enakatta);
    },

    // Branch 6: Any POS for ざる followed by を and 得 forms
    // Catch-all for unexpected GiNZA parsings
    (b) => {
      const zaru = b.tok({
        text: 'ざる',
      }, 'zaru');
      const wo = b.particle('を', 'wo');
      const enai = b.tok({
        text: /^(得(ない|ません|なかった|ませんでした))$/,
      }, 'enai');
      b.inOrder(zaru, wo, enai, 5);
      b.captureSpan('ざるを得ない', zaru, enai);
    }
  );
});
