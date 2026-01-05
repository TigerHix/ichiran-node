import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ずに (zuni) - "without doing"
 *
 * Verb negative stem + ずに = "without doing X"
 *
 * This is a classical/literary form of ないで (without doing).
 * The ず is the classical negative auxiliary (equivalent to ぬ/ない).
 * The に is a case particle making it adverbial.
 *
 * Formation:
 * - Remove ない from the negative form and attach ず(に)
 * - Five-grade verbs (五段動詞): 読まない → 読まず
 * - Ichidan verbs (一段動詞): 食べない → 食べず
 * - Irregular verbs:
 *   - する → せず (NOT しず)
 *   - 来る → 来ず
 *   - ある → ず (for negative stem constructions)
 *
 * Examples:
 * - 何も知らずにあんなこと言ってごめんなさい (Sorry for saying that without knowing)
 * - 朝ごはんを食べずに仕事に行きました (Went to work without eating breakfast)
 * - 水を飲まずに運動をしていたから (Was exercising without drinking water)
 * - 値段を見ずに買ったら大変なことになった (Bought it without checking the price)
 * - 忘れずに届けてくれて (Thank you for delivering without forgetting)
 * - 悩まずに生きるなんて (Living without worrying)
 * - 休まず、一日中ゲームをやり続けた (Played games all day without resting)
 * - 試合に一回も負けず、優勝した (Won the championship without losing once)
 * - 力まずにスウィングした (Swung without straining)
 * - 注文せずにチャーハンだけ食べました (Only ate fried rice without ordering ramen)
 * - 諦めずに続ければ (If you continue without giving up)
 * - 分からずに操作すると (If you operate without understanding)
 * - 気を緩めず、次の試合も頑張って (Don't lose focus, do your best in the next match too)
 * - 勉強せずにテストを受けた (Took the test without studying)
 * - 無理をせずに頑張ってくださいね (Please do your best without overdoing it)
 *
 * Note: The に can be omitted (e.g., 休まず, 負けず, 緩めず) making it just ず.
 *
 * GiNZA parse patterns:
 * 1. ず(に) as AUX with dep=aux attached to verb stem
 * 2. ず(に) as PART with dep=mark or dep=case
 * 3. ず(に) as SCONJ with dep=mark
 * 4. Various inflection forms and lemma values
 */
export default bunproLinguisticRule('ずに', (r) => {
  r.either(
    // Pattern 1: ずに/ず as auxiliary (AUX) with dep=aux
    // Most common pattern for classical auxiliary attached to verb
    (b) => {
      const zuni = b.aux({
        textOneOf: ['ずに', 'ず'],
        dep: 'aux',
      }, 'zuni');
      b.capture(zuni);
    },

    // Pattern 2: ずに/ず as particle (PART) with dep=mark or dep=case
    // GiNZA sometimes parses it as a case particle
    (b) => {
      const zuni = b.tok({
        textOneOf: ['ずに', 'ず'],
        pos: 'PART',
        depOneOf: ['mark', 'case'],
      }, 'zuni');
      b.capture(zuni);
    },

    // Pattern 3: ずに/ず as subordinating conjunction (SCONJ)
    // Alternative parsing for some contexts
    (b) => {
      const zuni = b.tok({
        textOneOf: ['ずに', 'ず'],
        pos: 'SCONJ',
      }, 'zuni');
      b.capture(zuni);
    },

    // Pattern 4: ずに/ず with any POS when lemma indicates classical negative
    // Catch-all for unexpected GiNZA parsings
    (b) => {
      const zuni = b.tok({
        textOneOf: ['ずに', 'ず'],
        // The classical auxiliary ず may have various lemmas in GiNZA
        lemmaOneOf: ['ず', 'ずに', 'ぬ'],
      }, 'zuni');
      b.capture(zuni);
    },

    // Pattern 5: ず with lemma=する for irregular する verb
    // する becomes せず, GiNZA may tag this differently
    (b) => {
      const zuni = b.tok({
        text: 'ず',
        lemma: 'する',
      }, 'zuni');
      b.capture(zuni);
    }
  );
});
