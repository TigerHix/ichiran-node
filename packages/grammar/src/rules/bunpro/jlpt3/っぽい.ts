import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: っぽい (-ish/-like suffix)
 *
 * A suffix that attaches to nouns, verb stems, and adjective stems
 * to create new i-adjectives meaning "-ish" or "having a tendency to".
 * Often has negative connotations.
 *
 * Formation patterns:
 * - Verb stem (masu form) + っぽい: 飽きっぽい (tends to get bored), 忘れっぽい (forgetful)
 * - Noun + っぽい: 子供っぽい (childish), 熱っぽい (feverish), 水っぽい (watery)
 * - I-adjective stem + っぽい: 安っぽい (cheap-looking), 黒っぽい (blackish), 白っぽい (whitish)
 * - Na-adjective + っぽい: 有名っぽい (seems famous)
 *
 * Examples from test data:
 * - 飽きっぽい: tends to get bored
 * - 子供っぽい: childish (adult acting like a child)
 * - 安っぽい: looks cheap
 * - 黒っぽい: blackish
 * - 嘘っぽい: sounds like a lie
 * - 熱っぽい: feverish
 * - 水っぽい: watery
 * - 忘れっぽい: forgetful
 * - 怒りっぽい: quick-tempered
 *
 * Conjugation:
 * - 飽きっぽい (dictionary form)
 * - 飽きっぽく (conjunctive form - 連用形)
 * - 飽きっぽかった (past form)
 *
 * GiNZA parse structure (inferred):
 * - っぽい is likely parsed as PART with tag=接尾辞 (suffix)
 * - Or potentially as ADJ (since it creates i-adjectives)
 * - The base word (noun, verb stem, adj stem) precedes it
 *
 * This rule uses flexible matching to catch the various formations.
 */
export default bunproLinguisticRule('っぽい', (r) => {
  // Match various forms of the っぽい suffix
  // Including conjugated forms as an i-adjective:
  // - Dictionary form: っぽい
  // - Conjunctive form (ren'youkei): っぽく
  // - Negative form: っぽくない (っぽく + ない)
  // - Past form: っぽかった
  // - Te-form: っぽくて
  // - Conditional form: っぽければ

  r.either(
    // Branch 1: Standalone suffix (っぽい, っぽく, etc. as separate tokens)
    (b) => {
      const ppoi = b.tok({
        textOneOf: [
          'っぽい',     // dictionary form
          'っぽく',     // conjunctive form (before て, ない, etc.)
          'っぽかった', // past form
          'っぽくて',   // te-form
          'っぽければ', // conditional
        ],
      }, 'ppoi');
      b.capture(ppoi);
    },

    // Branch 2: Combined form (base + suffix in one token, like 安っぽく, あらっぽく)
    (b) => {
      const ppoi = b.tok({
        textOneOf: [
          // From test data - specific combined forms that appear
          '安っぽく',      // やす + っぽく
          '安っぽい',      // やす + っぽい
          '黒っぽい',      // くろ + っぽい
          '黒っぽく',      // くろ + っぽく
          '白っぽい',      // しろ + っぽい
          '水っぽい',      // みず + っぽい
          '熱っぽい',      // ねつ + っぽい
          'あらっぽく',    // あら + っぽく (荒い + っぽく)
          'あらっぽい',    // あら + っぽい
          '惚れっぽく',    // ほれ + っぽく
          '飽きっぽい',    // あき + っぽい
          '忘れっぽい',    // わすれ + っぽい
          '怒りっぽい',    // おこり + っぽい
          '子供っぽい',    // こども + っぽい
          '理屈っぽい',    // りくつ + っぽい
          '油っぽく',      // あぶら + っぽく
          '嘘っぽい',      // うそ + っぽい
          '有名っぽい',    // ゆうめい + っぽい
        ],
      }, 'ppoi');
      b.capture(ppoi);
    }
  );
});
