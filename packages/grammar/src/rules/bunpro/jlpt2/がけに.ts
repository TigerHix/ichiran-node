import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: がけに (gake-ni) - "on the way to, as you go"
 *
 * Verb stem + がけに = "on the way doing X", "while in the course of X"
 *
 * Examples:
 * - 帰りがけに駅前のたこ焼き屋でたこ焼きを買った (On my way home, I bought takoyaki)
 * - 学校への行きがけにこの駄菓子屋によく立ち寄ったものだ (On my way to school...)
 * - 通りがけにサービスエリアにでも寄って行こう (Let's stop by the service area on the way)
 *
 * The pattern attaches to verb stems (masu form/ren'youkei):
 * - 帰り (kaeri) from 帰る (kaeru) - on the way home
 * - 行き (iki) from 行く (iku) - on the way
 * - 来 (ki) from 来る (kuru) - on the way here
 * - 通り (toori) from 通る (tooru) - on the way through
 *
 * According to Bunpro data:
 * "A phrase showing that something is done along the way. While suspended in the action of (A), (B)."
 * "On the way to, As you go"
 *
 * GiNZA may tokenize this in various ways:
 * - As single compound tokens (e.g., 帰りがけに as NOUN or ADV)
 * - As separate tokens (verb stem + がけに)
 * - Sometimes with kanji (掛けに)
 *
 * Common verb stems that combine with がけに:
 * - 帰り (kaeri) - from 帰る (to return/home)
 * - 行き/ゆき (iki/yuki) - from 行く (to go)
 * - 来 (ki) - from 来る (to come)
 * - 通り (toori) - from 通る (to pass through)
 */
export default linguisticRule('がけに', (r) => {
  r.either(
    // Branch 1: "verb stem + がけに" as single NOUN/ADV token
    // Handles tokenization like: [帰りがけに] where whole phrase is one token
    (b) => {
      const gakeni = b.noun({
        textOneOf: ['帰りがけに', 'かえりがけに', '行きがけに', 'いきがけに',
                     'ゆきがけに', '来がけに', 'きがけに', '通りがけに', 'とおりがけに']
      }, 'gakeni');
      b.capture(gakeni);
    },

    // Branch 2: "verb stem + がけに" as ADV token
    // Sometimes GiNZA tags these as ADV instead of NOUN
    (b) => {
      const gakeni = b.adv({
        textOneOf: ['帰りがけに', 'かえりがけに', '行きがけに', 'いきがけに',
                     'ゆきがけに', '来がけに', 'きがけに', '通りがけに', 'とおりがけに']
      }, 'gakeni');
      b.capture(gakeni);
    },

    // Branch 3: "verb stem + がけ" compound + に particle
    // Handles tokenization like: [帰りがけ] [に] where stem+gake is one NOUN token
    (b) => {
      const gake = b.noun({
        textOneOf: ['帰りがけ', 'かえりがけ', '行きがけ', 'いきがけ', 'ゆきがけ',
                     '来がけ', 'きがけ', '通りがけ', 'とおりがけ'],
        lemmaOneOf: ['帰りがけ', 'かえりがけ', '行きがけ', 'いきがけ', 'ゆきがけ',
                     '来がけ', 'きがけ', '通りがけ', 'とおりがけ']
      }, 'gake');
      const ni = b.particle('に', 'ni');
      b.inOrder(gake, ni, 1);
      b.captureSpan('がけに', gake, ni);
    },

    // Branch 4: がけ as NOUN + に particle
    // Handles tokenization like: [帰り] [がけ] [に] where stem is separate
    (b) => {
      const gake = b.noun({
        textOneOf: ['がけ', 'ガケ', '掛け', '掛け'],
        lemmaOneOf: ['がけ', '掛け']
      }, 'gake');
      const ni = b.particle('に', 'ni');
      b.inOrder(gake, ni, 1);
      // Any token before がけ
      const stem = b.tok({}, 'stem');
      b.inOrder(stem, gake, 1);
      b.captureSpan('がけに', stem, ni);
    },

    // Branch 5: がけに as single token (ADP/SCONJ or ADV)
    // GiNZA sometimes tokenizes the whole suffix as one token
    (b) => {
      const gakeni = b.tok({
        textOneOf: ['がけに', 'ガケに']
      }, 'gakeni');
      // Any token before it (the verb stem)
      const stem = b.tok({}, 'stem');
      b.inOrder(stem, gakeni, 1);
      b.captureSpan('がけに', stem, gakeni);
    },

    // Branch 6: Single token compound with common verb stems (lemma match)
    // GiNZA sometimes parses the whole "verb stem + がけに" as one token
    (b) => {
      const gakeni = b.tok({
        lemmaOneOf: [
          '帰りがけに', 'かえりがけに',
          '行きがけに', 'いきがけに', 'ゆきがけに',
          '来がけに', 'きがけに',
          '通りがけに', 'とおりがけに'
        ]
      }, 'gakeni');
      b.capture(gakeni);
    },

    // Branch 7: Kanji versions (掛けに) as single token
    (b) => {
      const gakeni = b.tok({
        textOneOf: ['掛けに', '掛けに']
      }, 'gakeni');
      // Any token before it (the verb stem)
      const stem = b.tok({}, 'stem');
      b.inOrder(stem, gakeni, 1);
      b.captureSpan('がけに', stem, gakeni);
    }
  );
});
