import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('各', (r) => {
  // 各 (かく/kaku) - prefix meaning "each", "every", "respective"
  // Read as かく (kaku) or それぞれ (sorezore)
  //
  // This is a slightly formal prefix that attaches to nouns (especially
  // Sino-Japanese compounds and katakana words) to mean "each/every" item
  // in a group or series.
  //
  // Examples from test data:
  // - 各バス停/かくバス停 (each bus stop)
  // - 各ページ/かくページ (each page)
  // - 各製品/かく製品 (each product)
  // - 各部屋/かく部屋 (each room)
  // - 各地/かく地 (each place/various places)
  // - 各モデル/かくモデル (each model)
  // - 各ファイル/かくファイル (each file)
  // - 各階/かく階 (each floor)
  // - 各イベント/かくイベント (each event)
  // - 各自/かく自 (each person/individually)
  // - 各アカウント/かくアカウント (each account)
  // - 各学年/かく学年 (each class/grade)
  // - 日本各地/日本かく地 (each place in Japan/all over Japan)
  //
  // Note: 各々（おのおの） is a related word meaning the same thing but
  // used primarily for people. It's a different grammar point.
  //
  // GiNZA analysis:
  // - Test data uses hiragana "かく" form (answers are filled in from cloze tests)
  // - Actual Japanese uses kanji "各" form
  // - Both are parsed as NOUN or PROPN with various dependencies
  // - Reading is typically 'かく' (kaku)
  //
  // Key discriminators:
  // - Must be either kanji 各 or hiragana かく

  // Match the prefix 各/かく
  const kaku = r.tok({ textOneOf: ['各', 'かく'] }, 'kaku');
  r.capture(kaku);
});
