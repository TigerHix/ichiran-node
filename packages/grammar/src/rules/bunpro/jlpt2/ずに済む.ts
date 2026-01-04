import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ずに済む (zu-ni-sumu) - "get away without doing", "manage to avoid doing"
 *
 * Verb negative stem + ず + に + 済む = "get by without doing X"
 *
 * This is an idiomatic expression using the classical negative auxiliary ず
 * (equivalent to ない/ぬ) + the particle に + the verb 済む (to finish/end).
 * Literally: "it ends without doing X" → "can get by without doing X"
 *
 * Formation:
 * - Remove ない from the negative form and attach ずに済む
 * - Five-grade verbs (五段動詞): 読まない → 読まずに済む
 * - Ichidan verbs (一段動詞): 食べない → 食べずに済む
 * - Irregular verbs:
 *   - する → せずに済む (NOT しずに済む)
 *   - 来る → 来ずに済む
 *
 * The verb 済む can be conjugated and may be written in hiragana:
 * - 済む/すむ (plain): 済まずに済む (can get by without doing)
 * - 済んだ/すんだ (past): 済まずに済んだ (got by without doing)
 * - 済みます/すみます (polite): 済まずに済みます (can get by without doing - polite)
 * - 済みました/すみました (past polite): 済まずに済みました (got by without doing - polite)
 *
 * Alternative forms (using ないで/なくて instead of ずに):
 * - 済まないで済む (more common/modern form)
 * - 渊まなくて済む (variant form)
 * - 済まずに済む (classical/literary form - this rule)
 *
 * Examples:
 * - 洗い物をせずに済む (get by without doing dishes)
 * - 今すぐ宿題をやっておけば後でやらずにすむ (if I do homework now, can get by without doing it later)
 * - このVIPパスを使えば待たずにすみます (if you use this VIP pass, can get by without waiting)
 * - 今月契約すると初期費用を支払わないで済む (if sign contract this month, get by without paying initial cost)
 * - 今年の冬は去年より暖かかったので、ヒーターなしで済みました (winter was warmer, so got by without heater)
 * - 犯人は警察から、捕まらずに済んだ (criminal got away without being caught by police)
 * - 芸能人がツイートしてくれたから、宣伝費を使わずに済んだ (celebrity tweeted, so got by without using ad expenses)
 * - ちゃんとペース配分したから、体力を消耗せずに済んだ (paced properly, so got by without exhausting physical strength)
 * - 運転士の巧みな技術のおかげで脱線せずに済んだ (thanks to driver's skill, got by without derailing)
 * - 知人の紹介で入社できたので、テストは受けずに済んだ (joined through acquaintance, so got by without taking test)
 * - 事故が起きたけど、怪我をせずに済んだ (accident occurred but got away without getting injured)
 * - 長女の的確な意見のおかげで、中途半端な決定をせずに済みました (thanks to oldest daughter's opinion, got by without half-baked decision)
 * - あの通訳さんのおかげで、海外での仕事で失敗せずに済みました (thanks to interpreter, got by without failing overseas)
 * - 着々と計画通り進んだので、口を出さずに済んだ (progressed as planned, so got by without having to say anything)
 * - 君が予定を合わせてくれたら、みんなのスケジュールを変更せずにすむ (if you match plans, everyone can get by without changing schedule)
 * - 基礎からトレーニングを積んでいたから、潰れずに済んだ (built from foundation, so got by without being crushed)
 *
 * Key discriminators:
 * - ず is the classical negative auxiliary (literary form of ない/ぬ)
 * - Must be followed by particle に
 * - Followed by conjugated form of 済む/すむ (sumu - to finish/end)
 * - Different from simple ずに (without doing) - this is specifically about "getting by without"
 * - The nuance is relief at avoiding something unpleasant
 *
 * GiNZA parse patterns:
 * - ず: AUX or PART, dep=aux, attaches to verb stem
 * - に: ADP/PART, dep=mark or dep=fixed, follows ず
 * - 済む/すむ/済んだ/すんだ/etc: VERB, lemma=済む or すむ
 * - Various conjugations: 済む/すむ (plain), 済んだ/すんだ (past), 済みます/すみます (polite), 済みました/すみました (past polite)
 */
export default linguisticRule('ずに済む', (r) => {
  r.either(
    // Pattern 1: ず + に + 済む/すむ (plain present)
    // Most common pattern: verb negative stem + ずに + 済む
    (b) => {
      const zu = b.tok({
        text: 'ず',
        posOneOf: ['AUX', 'PART', 'SCONJ'],
      }, 'zu');

      const ni = b.tok({
        text: 'に',
        posOneOf: ['ADP', 'PART'],
        depOneOf: ['mark', 'case', 'fixed'],
      }, 'ni');

      b.inOrder(zu, ni, 2);

      const sumu = b.verb({
        lemmaOneOf: ['済む', 'すむ'],
        textOneOf: ['済む', 'すむ'],
      }, 'sumu');

      b.inOrder(ni, sumu, 5);

      b.captureSpan('ずに済む', zu, sumu);
    },

    // Pattern 2: ず + に + 済んだ/すんだ (past tense)
    (b) => {
      const zu = b.tok({
        text: 'ず',
        posOneOf: ['AUX', 'PART', 'SCONJ'],
      }, 'zu');

      const ni = b.tok({
        text: 'に',
        posOneOf: ['ADP', 'PART'],
        depOneOf: ['mark', 'case', 'fixed'],
      }, 'ni');

      b.inOrder(zu, ni, 2);

      const sumu = b.verb({
        lemmaOneOf: ['済む', 'すむ'],
        textOneOf: ['済んだ', 'すんだ'],
      }, 'sumu');

      b.inOrder(ni, sumu, 5);

      b.captureSpan('ずに済む', zu, sumu);
    },

    // Pattern 3: ず + に + 済みます/すみます (polite present)
    (b) => {
      const zu = b.tok({
        text: 'ず',
        posOneOf: ['AUX', 'PART', 'SCONJ'],
      }, 'zu');

      const ni = b.tok({
        text: 'に',
        posOneOf: ['ADP', 'PART'],
        depOneOf: ['mark', 'case', 'fixed'],
      }, 'ni');

      b.inOrder(zu, ni, 2);

      const sumu = b.verb({
        lemmaOneOf: ['済む', 'すむ'],
        textOneOf: ['済みます', 'すみます'],
      }, 'sumu');

      b.inOrder(ni, sumu, 5);

      b.captureSpan('ずに済む', zu, sumu);
    },

    // Pattern 4: ず + に + 済みました/すみました (past polite)
    (b) => {
      const zu = b.tok({
        text: 'ず',
        posOneOf: ['AUX', 'PART', 'SCONJ'],
      }, 'zu');

      const ni = b.tok({
        text: 'に',
        posOneOf: ['ADP', 'PART'],
        depOneOf: ['mark', 'case', 'fixed'],
      }, 'ni');

      b.inOrder(zu, ni, 2);

      const sumu = b.verb({
        lemmaOneOf: ['済む', 'すむ'],
        textOneOf: ['済みました', 'すみました'],
      }, 'sumu');

      b.inOrder(ni, sumu, 5);

      b.captureSpan('ずに済む', zu, sumu);
    },

    // Pattern 5: More flexible - any form of 済む/すむ after ずに
    // Catch-all for other conjugations or parsing variations
    (b) => {
      const zu = b.tok({
        text: 'ず',
      }, 'zu');

      const ni = b.tok({
        text: 'に',
      }, 'ni');

      b.inOrder(zu, ni, 3);

      const sumu = b.verb({
        lemmaOneOf: ['済む', 'すむ'],
      }, 'sumu');

      b.inOrder(ni, sumu, 8);

      b.captureSpan('ずに済む', zu, sumu);
    }
  );
});
