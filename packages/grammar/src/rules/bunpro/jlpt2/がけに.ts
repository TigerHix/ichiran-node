import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: がけに (gakeni) - On the way to, as you go
 *
 * A suffix meaning "on the way" or "while in the course of". Attached to the
 * stem (masu-form) of movement verbs to indicate that something happens during
 * the course of that movement. Expresses doing something while going/coming/transiting.
 *
 * Structure:
 * - Verb［stem/masu form］+ がけに
 *
 * Examples:
 * - 帰りがけに駅前のたこ焼き屋でたこ焼きを買った。
 *   (On the way home, I bought takoyaki at a takoyaki store in front of the station.)
 * - 学校への行きがけにコンビニで弁当を買う。
 *   (On the way to school, I will buy lunch at the convenience store.)
 * - 通りがけにサービスエリアにでも寄って行こう！
 *   (Let's stop by the service area on the way!)
 * - さっきあいつ、来がけに妙なことを言ってたね。
 *   (Just now, on the way here, that dude was saying weird things.)
 *
 * Key discriminators:
 * - がけに attaches to verb stem (連用形/masu stem)
 * - Used with movement verbs (帰る, 行く, 来る, 通る, etc.)
 * - Final に is a particle, not part of the suffix
 * - Less common in modern Japanese (formal/literary)
 * - Different from 途中で/途中に (attaches to dictionary form)
 * - Different from ついでに (opportunity/convenience focus)
 *
 * GiNZA parse structure:
 * - 帰り(NOUN/VERB stem) + がけに(NOUN/ADV) - single token or compound
 * - OR 帰り(VERB stem) + がけ(NOUN) + に(ADP)
 * - Various dependency patterns (compound, fixed, advcl)
 */
export default linguisticRule('がけに', (r) => {
  r.either(
    // Branch 1: がけに as single compound token after verb stem
    // Most common: 帰りがけに, 行きがけに parsed together
    (b) => {
      const gakeni = b.tok({
        textOneOf: ['がけに', '掛けに'],
        lemma: 'がけに',
      }, 'gakeni');
      b.capture(gakeni);
    },

    // Branch 2: Verb stem + がけ + に with compound/fixed dependency
    // Alternative parsing where がけ and に are separate
    (b) => {
      const gake = b.tok({
        textOneOf: ['がけ', '掛け'],
        lemma: 'がけ',
      }, 'gake');
      const ni = b.particle('に', 'ni');
      b.inOrder(gake, ni, 1);
      b.captureSpan('がけに', gake, ni);
    },

    // Branch 3: Verb stem + がけ with advcl or compound dependency
    // Stem is syntactic head, がけ modifies it
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const gake = b.tok({
        textOneOf: ['がけ', '掛け', 'がけに', '掛けに'],
        lemmaOneOf: ['がけ', 'がけに'],
      }, 'gake');
      b.headChild(stem, gake, 'advcl');
      b.captureSpan('がけに', stem, gake);
    },

    // Branch 4: Verb stem + がけ with compound dependency
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const gake = b.tok({
        textOneOf: ['がけ', '掛け', 'がけに', '掛けに'],
        lemmaOneOf: ['がけ', 'がけに'],
      }, 'gake');
      b.headChild(stem, gake, 'compound');
      b.captureSpan('がけに', stem, gake);
    },

    // Branch 5: Verb stem followed by がけ(に) with fixed dependency
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const gake = b.tok({
        textOneOf: ['がけ', '掛け', 'がけに', '掛けに'],
        lemmaOneOf: ['がけ', 'がけに'],
      }, 'gake');
      b.headChild(stem, gake, 'fixed');
      b.captureSpan('がけに', stem, gake);
    }
  );
});
