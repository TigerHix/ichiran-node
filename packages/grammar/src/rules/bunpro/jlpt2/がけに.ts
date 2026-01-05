import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: がけに (gake ni) - On the way to, As you go
 *
 * A suffix attached to verb stems meaning "while in the course of" or "on the way".
 * Indicates that something happens during the action (A). (B) may be done while
 * doing (A), or specifically because (A) presents a good opportunity.
 *
 * Structure:
 * - Verb［stem］+ がけに
 *
 * Examples:
 * - 帰りがけに駅前のたこ焼き屋でたこ焼きを買った。
 *   (On my way home, I bought some takoyaki from the takoyaki store in front of the station.)
 * - 学校への行きがけにコンビニで弁当を買う。
 *   (On my way to school, I will buy lunch at the convenience store.)
 * - 通りがけにサービスエリアに寄って行こう。
 *   (Let's stop by the service area on the way.)
 * - 来がけに妙なことを言ってたね。
 *   (He was saying weird things on the way here.)
 *
 * Key discriminators:
 * - がけに is a suffix attached to verb stems (masu-stem/ren'youkei)
 * - Used primarily with movement verbs (行く, 来る, 帰る, 通る, etc.)
 * - The particle に always follows がけ
 * - Different from 途中で/途中に (attaches to dictionary form)
 * - Different from ついでに (focuses on opportunity/convenience)
 *
 * GiNZA parse structure:
 * - 帰りがけに: 帰り(VERB,inf=連用形) + がけ(NOUN/PART) + に(ADP)
 * - 行きがけに: 行き(VERB,inf=連用形) + がけ(NOUN/PART) + に(ADP)
 *
 * Note: がけ can be parsed as NOUN or PART by GiNZA
 */
export default linguisticRule('がけに', (r) => {
  // Verb stem + がけ + に
  const stem = r.tok({
    posOneOf: ['VERB', 'ADJ'],
    inflectionForm: '連用形-一般',
  }, 'stem');

  // The suffix がけ (can be NOUN or PART depending on GiNZA's parsing)
  const gake = r.tok({
    textOneOf: ['がけ', '掛け'],
    posOneOf: ['NOUN', 'PART'],
  }, 'gake');

  r.inOrder(stem, gake, 2);

  // Followed by particle に
  const ni = r.tok({
    text: 'に',
    pos: 'ADP',
  }, 'ni');

  r.inOrder(gake, ni, 1);

  r.captureSpan('がけに', stem, ni);
});
