import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: それで - Therefore / So / And then
 *
 * Matches それで (sorede) used as a conjunction meaning "therefore", "so", or "and then".
 * Used at the beginning of a sentence to connect to previous context.
 *
 * Structure:
 * - それ (demonstrative pronoun) + で (particle)
 * - それで (therefore/so)
 *
 * Examples:
 * - JLPTに合格したいです。それで毎日しっかり勉強しています。
 *   (I want to pass the JLPT. Therefore, I study hard every day.)
 * - 昨日、私たちは初めてデートしたわ。へえーそうなんだ。それでどうだった？
 *   (Yesterday, we went on a date for the first time. Oh yeah? And then? What happened?)
 * - 部屋は真っ暗だった。それで彼は何も見えなかった。
 *   (The room was pitch black. Therefore, he couldn't see anything.)
 *
 * Key discriminators:
 * - Requires それ + で adjacent (text constraint)
 * - Distinguished from それ + other particles (それを, それは, etc.)
 *
 * Note: In Japanese, the instrumental usage "WITH that" (e.g., "wrote with that")
 * would typically use それを使って rather than それで. The conjunctive それで
 * is primarily used at sentence beginnings.
 *
 * GiNZA parse structure:
 * - それで: それ(CCONJ/PRON) + で(particle)
 * - Both POS variants (CCONJ for conjunction, PRON for pronoun) are accepted
 */
export default linguisticRule('それで', (r) => {
  // それで as conjunction (pronoun + particle)
  // GiNZA parses this as either CCONJ or PRON + particle
  r.either(
    // Pattern 1: それ (CCONJ) + で
    (b) => {
      const sore = b.tok({ text: 'それ', pos: 'CCONJ' }, 'sore');
      const de = b.tok({ text: 'で' }, 'de');
      b.inOrder(sore, de, 1);
      b.captureSpan('それで', sore, de);
    },
    // Pattern 2: それ (PRON) + で
    (b) => {
      const sore = b.tok({ text: 'それ', pos: 'PRON' }, 'sore');
      const de = b.tok({ text: 'で' }, 'de');
      b.inOrder(sore, de, 1);
      b.captureSpan('それで', sore, de);
    }
  );
});
