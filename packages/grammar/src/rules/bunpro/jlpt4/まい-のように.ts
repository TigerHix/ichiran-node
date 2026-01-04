import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: まい～のように - Almost every ~, Nearly every ~, On a ~ basis
 *
 * Matches patterns where まい (every) + time expression + のように (like/manner of)
 * expresses "almost every (time period)".
 *
 * Structure:
 * - まい (hiragana for 'every') + time expression + の + ように
 *
 * Examples:
 * - まい週のように行っている (go almost every week)
 * - まい年のように平和記念資料館へ行く (go to peace memorial museum almost every year)
 * - まい日のように高校をサボっていた (skipped school almost every day)
 * - まい晩のようにジムに行く (go to gym almost every night)
 * - まいとしのように (all hiragana variant)
 *
 * Key discriminators:
 * - まい (hiragana for 'every') followed by time expression
 * - Followed by particle の and ように
 * - Expresses frequency "almost every (time period)"
 *
 * Common time expressions:
 * - まい日 (まいにち) - every day
 * - まい週 (まいしゅう) - every week
 * - まい月 (まいつき) - every month
 * - まい年 (まいとし) - every year (all hiragana: まいとし)
 * - まい晩 (まいばん) - every night (all hiragana: まいばん)
 * - まい朝 (まいあさ) - every morning
 * - まい回 (まいかい) - every time
 *
 * Note: This pattern uses hiragana まい (not kanji 毎). The time expression can be:
 * - A single kanji: まい日, まい週, etc. (sometimes parsed as single token by GiNZA)
 * - Full hiragana: まいとし, まいばん, まいあさ, etc.
 *
 * GiNZA can parse variably:
 * - まい日 as one token or as まい + 日
 * - ように as one token or as よう + に
 *
 * Some GiNZA parsing variations have been observed where different sentences
 * with the same pattern are parsed differently. This rule uses `r.either()` to
 * handle the variations.
 */
export default linguisticRule('まい-のように', (r) => {
  r.either(
    // Branch 1: Pattern with tokens close together (most common)
    (b) => {
      const mai = b.tok({ text: 'まい' }, 'mai');
      const timeExpr = b.tok({}, 'timeExpr');
      const no = b.particle('の', 'no');
      const yoni = b.tok({ text: 'ように' }, 'yoni');

      b.inOrder(mai, timeExpr, 3);
      b.inOrder(timeExpr, no, 3);
      b.inOrder(no, yoni, 3);
      b.captureSpan('まい-のように', mai, yoni);
    },
    // Branch 2: Pattern with よう + に separate
    (b) => {
      const mai = b.tok({ text: 'まい' }, 'mai');
      const timeExpr = b.tok({}, 'timeExpr');
      const no = b.particle('の', 'no');
      const you = b.tok({ text: 'よう' }, 'you');
      const ni = b.particle('に', 'ni');

      b.inOrder(mai, timeExpr, 3);
      b.inOrder(timeExpr, no, 3);
      b.inOrder(no, you, 3);
      b.inOrder(you, ni, 1);
      b.captureSpan('まい-のように', mai, ni);
    }
  );
});
