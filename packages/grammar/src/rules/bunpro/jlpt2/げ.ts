import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: げ (appearance, signs of, seeming)
 *
 * A suffix that attaches to adjective stems, verb stems, and nouns
 * to express "giving the appearance of" or "showing signs of".
 * More subjective and low-confidence than そう.
 *
 * Formation patterns:
 * - Verb stem (masu form) + げ: 待ちげ (looking like they're waiting)
 * - I-adjective stem (-い removed) + げ: 悲しげ (sad-looking), 楽しげ (happy-looking)
 * - Na-adjective/noun + げ: 満足げ (satisfied-looking), 不安げ (anxious-looking)
 *
 * Examples from test data:
 * - 悲しげな顔: a sad-looking face
 * - 楽しげに遊ぶ: seemingly enjoying playing
 * - 不安げに: seemingly nervous
 * - くやしげな表情: frustrated-looking expression
 * - なつかしげに: nostalgically
 * - はずかしげに: seemingly embarrassed
 * - かなしげな: sad-looking
 * - うらやましげに: enviously
 * - ありげな: meaningful-looking
 * - まんぞくげな: satisfied-looking
 * - すずしげな: seemingly unruffled/cool
 * - あやしげな: suspicious-looking
 * - かわいげ: cuteness (from 可愛い, noun use)
 *
 * The suffix creates na-adjectives, so it's followed by な or に, or ends with だ.
 *
 * GiNZA parse structure (inferred):
 * - げ is parsed as a suffix tag (接尾辞)
 * - The base word (stem) precedes it
 * - May be followed by particles like な, に, or copula だ
 */
export default linguisticRule('げ', (r) => {
  // Match the げ suffix in various forms
  // It creates a na-adjective, so it appears as:
  // - げ alone (followed by other elements)
  // - げな (modifying a noun)
  // - げに (modifying a verb)
  // - げだ (copula)

  r.either(
    // Branch 1: Standalone suffix げ (followed by な, に, だ)
    (b) => {
      const ge = b.tok({
        textOneOf: ['げ', 'ゲ'],
        tag: '接尾辞-形状詞的', // na-adjective-like suffix tag
      }, 'ge');
      b.capture(ge);
    },

    // Branch 2: Combined forms (stem + げ in one token)
    // These are common in GiNZA parsing for adjective-derived forms
    (b) => {
      const ge = b.tok({
        textOneOf: [
          // I-adjective stem + げ forms
          '悲しげ',     // 悲しい + げ
          '悲しげな',   // 悲しい + げ + な
          '悲しげに',   // 悲しい + げ + に
          '楽しげ',     // 楽しい + げ
          '楽しげに',   // 楽しい + げ + に
          '楽しげな',   // 楽しい + げ + な
          '悔しげな',   // くやしい + げ + な
          '悔しげ',     // くやしい + げ
          '恥ずかしげに', // はずかしい + げ + に
          '恥ずかしげ',   // はずかしい + げ
          '懐かしげに', // なつかしい + げ + に
          '懐かしげ',   // なつかしい + げ
          '儚げな',     // はかない + げ + な
          '儚げ',       // はかない + げ
          '涼しげな',   // すずしい + げ + な
          '涼しげ',     // すずしい + げ
          '羡ましげに', // うらやましい + げ + に
          '羡ましげ',   // うらやましい + げ
          'ありげな',   // ある + げ + な
          'ありげ',     // ある + げ
          '怪しげな',   // あやしい + げ + な
          '怪しげ',     // あやしい + げ
          '満足げな',   // まんぞく + げ + な
          '満足げ',     // まんぞく + げ
          '不安げに',   // ふあん + げ + に
          '不安げ',     // ふあん + げ
          'かわいげ',   // かわいい + げ (noun form)
        ],
      }, 'ge');
      b.capture(ge);
    }
  );
});
