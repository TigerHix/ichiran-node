import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('る-Verbs', (r) => {
  // る-verbs (ichidan verbs): single-conjugation verbs ending in る
  // All conjugation classes starting with 下一段 or 一段 (ichidan)
  //
  // This rule matches ichidan verbs by providing representative example
  // lemmas for each conjugation class. The rule dispatches on these lemmas.

  r.either(
    // 下一段-カ行 (e.g., 受ける, あげる, 閉める, 出る, 捨てる)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['受ける', 'あげる', '閉める'],
          conjugationClass: '下一段-カ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 下一段-ガ行 (e.g., 上げる)
    (b) => {
      const verb = b.verb(
        {
          lemma: '上げる',
          conjugationClass: '下一段-ガ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 下一段-サ行 (e.g., 出る)
    (b) => {
      const verb = b.verb(
        {
          lemma: '出る',
          conjugationClass: '下一段-サ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 下一段-タ行 (e.g., 捨てる)
    (b) => {
      const verb = b.verb(
        {
          lemma: '捨てる',
          conjugationClass: '下一段-タ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 下一段-バ行 (e.g., 読める)
    (b) => {
      const verb = b.verb(
        {
          lemma: '読める',
          conjugationClass: '下一段-バ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 下一段-マ行 (e.g., 飲める)
    (b) => {
      const verb = b.verb(
        {
          lemma: '飲める',
          conjugationClass: '下一段-マ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 下一段-ラ行 (e.g., 食べる, 寝る, 教える, 始める, 開ける, つける, 答える)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['食べる', '寝る', '教える', '始める', '開ける', 'つける', '答える'],
          conjugationClass: '下一段-ラ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 上一段-カ行 (e.g., 着る)
    (b) => {
      const verb = b.verb(
        {
          lemma: '着る',
          conjugationClass: '上一段-カ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 上一段-ガ行 (e.g., 過ぎる)
    (b) => {
      const verb = b.verb(
        {
          lemma: '過ぎる',
          conjugationClass: '上一段-ガ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 上一段-タ行 (e.g., 落ちる)
    (b) => {
      const verb = b.verb(
        {
          lemma: '落ちる',
          conjugationClass: '上一段-タ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 上一段-ナ行 (e.g., 似る)
    (b) => {
      const verb = b.verb(
        {
          lemma: '似る',
          conjugationClass: '上一段-ナ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 上一段-バ行 (e.g., 減びる)
    (b) => {
      const verb = b.verb(
        {
          lemma: '減びる',
          conjugationClass: '上一段-バ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    },
    // 上一段-ラ行 (e.g., 見る, いる, できる, 起きる, 感じる, 借りる, 降りる)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['見る', 'いる', 'できる', '起きる', '感じる', '借りる', '降りる'],
          conjugationClass: '上一段-ラ行',
        },
        'ruVerb'
      );
      b.capture(verb);
    }
  );
});
