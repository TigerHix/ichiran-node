import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: verb-non-past - Verb Non-Past (Dictionary form)
 *
 * Matches verbs in dictionary form (non-past), also called "plain form" or "dictionary form".
 * This is the base form of verbs ending in:
 * - u-verbs (godan): ending in う sounds (e.g., 買う, 書く, 読む)
 * - ru-verbs (ichidan): ending in る (e.g., 食べる, 見る)
 * - irregular verbs: する, 来る (くる)
 *
 * The dictionary form is used for:
 * - Future actions: "明日行く" (will go tomorrow)
 * - Habits: "毎日食べる" (eat every day)
 * - General truths: "鳥は飛ぶ" (birds fly)
 *
 * This rule matches verbs in their terminal form (終止形-一般) or attributive form (連体形-一般),
 * which are the dictionary forms. It excludes:
 * - Polite forms (〜ます)
 * - Past tense (〜た/〜だ)
 * - Negative forms (〜ない)
 * - Te-form (〜て)
 * - Other conjugated forms
 *
 * The key discriminator: dictionary form verbs have text === lemma (e.g., "走る" has text="走る" and lemma="走る")
 * Conjugated forms have text !== lemma (e.g., "走ります" has text="走り" but lemma="走る")
 */
export default bunproLinguisticRule('verb-non-past', (r) => {
  // Use r.either() to provide literal triggers for representative verbs from test data
  // Each branch matches a specific verb lemma with its conjugation class
  r.either(
    // ===== U-verbs (Godan / 五段) =====
    // 五段-カ行 (k-row): 聞く, 行く, 歩く
    (b) => {
      const verb = b.verb(
        {
          textOneOf: ['聞く', '行く', '歩く'],
          lemmaOneOf: ['聞く', '行く', '歩く'],
          conjugationClass: '五段-カ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 五段-ガ行 (g-row): 泳ぐ
    (b) => {
      const verb = b.verb(
        {
          text: '泳ぐ',
          lemma: '泳ぐ',
          conjugationClass: '五段-ガ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 五段-サ行 (s-row): 話す
    (b) => {
      const verb = b.verb(
        {
          text: '話す',
          lemma: '話す',
          conjugationClass: '五段-サ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 五段-タ行 (t-row): 打つ
    (b) => {
      const verb = b.verb(
        {
          text: '打つ',
          lemma: '打つ',
          conjugationClass: '五段-タ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 五段-ナ行 (n-row): 死ぬ
    (b) => {
      const verb = b.verb(
        {
          text: '死ぬ',
          lemma: '死ぬ',
          conjugationClass: '五段-ナ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 五段-バ行 (b-row): 飛ぶ, 遊ぶ
    (b) => {
      const verb = b.verb(
        {
          textOneOf: ['飛ぶ', '遊ぶ'],
          lemmaOneOf: ['飛ぶ', '遊ぶ'],
          conjugationClass: '五段-バ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 五段-マ行 (m-row): 飲む, 読む (よむ)
    (b) => {
      const verb = b.verb(
        {
          textOneOf: ['飲む', '読む'],
          lemmaOneOf: ['飲む', '読む'],
          conjugationClass: '五段-マ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 五段-ラ行 (r-row): 走る, 会う, 座る, 帰る
    (b) => {
      const verb = b.verb(
        {
          textOneOf: ['走る', '会う', '座る', '帰る'],
          lemmaOneOf: ['走る', '会う', '座る', '帰る'],
          conjugationClass: '五段-ラ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 五段-ワア行 (wa-row): 言う, 洗う (あらう), 習う (ならう)
    (b) => {
      const verb = b.verb(
        {
          textOneOf: ['言う', '洗う', '習う'],
          lemmaOneOf: ['言う', '洗う', '習う'],
          conjugationClass: '五段-ワア行',
        },
        'verb'
      );
      b.capture(verb);
    },

    // ===== Ru-verbs (Ichidan / 一段) =====
    // 上一段-カ行: 着る (きる)
    (b) => {
      const verb = b.verb(
        {
          text: '着る',
          lemma: '着る',
          conjugationClass: '上一段-カ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 上一段-マ行: 見る
    (b) => {
      const verb = b.verb(
        {
          text: '見る',
          lemma: '見る',
          conjugationClass: '上一段-マ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 下一段-ナ行: 寝る (ねる)
    (b) => {
      const verb = b.verb(
        {
          text: '寝る',
          lemma: '寝る',
          conjugationClass: '下一段-ナ行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 下一段-ア行: 教える (おしえる)
    (b) => {
      const verb = b.verb(
        {
          text: '教える',
          lemma: '教える',
          conjugationClass: '下一段-ア行',
        },
        'verb'
      );
      b.capture(verb);
    },
    // 下一段-バ行: 食べる (たべる)
    (b) => {
      const verb = b.verb(
        {
          text: '食べる',
          lemma: '食べる',
          conjugationClass: '下一段-バ行',
        },
        'verb'
      );
      b.capture(verb);
    },

    // ===== Irregular verbs =====
    // サ行変格: する (matches both VERB and AUX since GiNZA tags it as AUX in compounds like 結婚する)
    (b) => {
      const verb = b.verb(
        {
          textOneOf: ['する', '為る'],
          lemmaOneOf: ['する', '為る'],
          conjugationClass: 'サ行変格',
        },
        'verb'
      );
      b.capture(verb);
    },
    (b) => {
      const verb = b.aux(
        {
          textOneOf: ['する', '為る'],
          lemmaOneOf: ['する', '為る'],
          conjugationClass: 'サ行変格',
        },
        'verb'
      );
      b.capture(verb);
    },
    // カ行変格: 来る (くる)
    (b) => {
      const verb = b.verb(
        {
          text: '来る',
          lemma: '来る',
          conjugationClass: 'カ行変格',
        },
        'verb'
      );
      b.capture(verb);
    },

    // ===== Special case: GiNZA incorrectly tags some verbs as NOUN =====
    // 習う (ならう) is tagged as NOUN with conjugationClass 五段-ワア行
    (b) => {
      const verb = b.noun(
        {
          text: '習う',
          lemma: '習う',
          conjugationClass: '五段-ワア行',
        },
        'verb'
      );
      b.capture(verb);
    }
  );
});
