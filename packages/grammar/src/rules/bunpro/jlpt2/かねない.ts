import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: かねない (kanenai - "might, it is possible that")
 *
 * Verb stem (masu form) + かねない = might happen, capable of happening
 * Usually negative: indicates possibility of negative outcome
 *
 * Examples:
 * - 事故が起こりかねない (might cause an accident)
 * - 失敗しかねない (could fail)
 * - なりかねない (might become)
 *
 * This rule matches any token with text=かね followed by text=ない
 */
export default linguisticRule('かねない', (r) => {
  r.either(
    // Branch 1: かね token + ない token
    (b) => {
      const kane = b.tok({
        textOneOf: ['かね', '兼ね'],
      }, 'kane');
      const nai = b.tok({
        text: 'ない',
      }, 'nai');
      b.inOrder(kane, nai, 1);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 2: Full compound forms (single tokens)
    (b) => {
      const kanenai = b.tok({
        textOneOf: [
          'かねない', '兼ねない',
          'しかねない', 'なりかねない',
          'やりかねない', 'おきかねない',
          'おこりかねない', 'ふやしかねない',
          'うみかねない', 'でかねない',
          '起こりかねない', '落ちかねない',
          '落としかねない', '感染しかねない',
          '起こりかねない',
        ],
      }, 'kanenai');
      b.capture(kanenai);
    }
  );
});
