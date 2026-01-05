import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: おおよそ (ooyoso/oyoso) - "approximately, roughly, about, outline, gist"
 *
 * A slightly formal adverb often used in announcements to indicate something
 * that is not exactly (A), but expected to be close to it. Commonly used with
 * time, distance, quantities, and estimates. Can also be used as a noun meaning
 * "outline" or "gist".
 *
 * Structures:
 * - おおよそ + Number/Quantity (adverbial)
 * - おおよそ + の + Noun (pre-nominal adverb)
 * - Noun + の + おおよそ (noun meaning "outline/gist")
 * - おおよそ alone (adverbial)
 *
 * Examples:
 * - おおよそ１０年前だ。
 *   (It was approximately 10 years ago.)
 * - およそ３０分です。
 *   (It's approximately 30 minutes.)
 * - おおよその人数を把握する。
 *   (To grasp the approximate number of people.)
 * - まずは計画のおおよそを説明しよう。
 *   (Let me explain the outline of the plan first.)
 *
 * Key discriminators:
 * - Can be written as おおよそ or およそ (hiragana)
 * - Can be written as 大凡 or 凡そ (kanji)
 * - POS is ADV (adverb) or NOUN (noun meaning "outline")
 * - Used to modify quantities, time, distance
 * - More formal than だいたい
 *
 * GiNZA parse structure:
 * - おおよそ/およそ/大凡/凡そ (ADV) - adverb
 * - おおよそ/およそ/大凡/凡そ (NOUN) - noun (outline/gist)
 * - May be followed by:
 *   - NUM (number)
 *   - NOUN (quantity noun)
 *   - の (ADP) when pre-nominal (おおよその人数)
 * - May be preceded by:
 *   - NOUN + の (noun phrase for "outline of X")
 *
 * Different from:
 * - だいたい (daitai) - more casual, "mostly/roughly"
 * - ほぼ (hobo) - "almost/nearly" (closer to exact)
 */
export default linguisticRule('おおよそ', (r) => {
  r.either(
    // Pattern 1: Noun + の + おおよそ (noun meaning "outline/gist of X")
    // Example: 計画のおおよそ (outline of the plan)
    (b1) => {
      const ooyoso = b1.tok({
        lemmaOneOf: ['おおよそ', 'およそ', '大凡', '凡そ'],
        pos: 'NOUN',
      }, 'ooyoso');
      const no = b1.particle('の', 'no');
      const noun = b1.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');

      b1.inOrder(noun, no, 1);
      b1.inOrder(no, ooyoso, 1);
      b1.captureSpan('おおよそ', noun, ooyoso);
    },

    // Pattern 2: おおよそ/およそ + の + Noun (pre-nominal adverb)
    // Example: おおよその人数, およその見積もり
    (b2) => {
      const ooyoso = b2.tok({
        lemmaOneOf: ['おおよそ', 'およそ', '大凡', '凡そ'],
        pos: 'ADV',
      }, 'ooyoso');
      const no = b2.particle('の', 'no');
      const noun = b2.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');

      b2.inOrder(ooyoso, no, 1);
      b2.inOrder(no, noun, 1);
      b2.captureSpan('おおよそ', ooyoso, noun);
    },

    // Pattern 3: おおよそ/およそ alone (adverbial use or noun)
    // Example: おおよそ１０年前, およそ３０分, おおよそ理解した
    (b3) => {
      const ooyoso = b3.tok({
        lemmaOneOf: ['おおよそ', 'およそ', '大凡', '凡そ'],
        posOneOf: ['ADV', 'NOUN'],
      }, 'ooyoso');

      b3.capture(ooyoso);
    }
  );
});
