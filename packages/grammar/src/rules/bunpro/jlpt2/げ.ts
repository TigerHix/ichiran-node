import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: げ (ge) - "appearance, sign, look of"
 *
 * A suffix for adjectives/verbs creating "seemingly" forms.
 * More subjective than そう.
 */
export default linguisticRule('げ', (r) => {
  r.either(
    // Pattern 1: Combined forms with な
    (b1) => {
      const geForm = b1.tok({
        textOneOf: [
          'くやしげな', 'まんぞくげな', 'あやしげな', 'かなしげな',
          'ありげな', 'はかなげな', 'すずしげな',
        ],
      }, 'geForm');
      b1.capture(geForm);
    },

    // Pattern 2: Combined forms with に
    (b2) => {
      const geForm = b2.tok({
        textOneOf: [
          'ありげに', '楽しげに', '不安げに', 'なつかしげに',
          'はずかしげに', 'たのしげに', 'うらやましげに',
        ],
      }, 'geForm');
      b2.capture(geForm);
    },

    // Pattern 3: Noun forms (no particle)
    (b3) => {
      const geForm = b3.tok({
        textOneOf: ['かわいげ', 'ありげ'],
      }, 'geForm');
      b3.capture(geForm);
    }
  );
});
