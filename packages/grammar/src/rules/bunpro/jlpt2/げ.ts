import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('げ', (r) => {
  r.either(
    (b1) => {
      const geForm = b1.tok({
        textOneOf: [
          'くやしげな', 'まんぞくげな', 'あやしげな', 'かなしげな',
          'ありげな', 'はかなげな', 'すずしげな',
        ],
      }, 'geForm');
      b1.capture(geForm);
    },
    (b2) => {
      const geForm = b2.tok({
        textOneOf: [
          'ありげに', '楽しげに', '不安げに', 'なつかしげに',
          'はずかしげに', 'たのしげに', 'うらやましげに',
        ],
      }, 'geForm');
      b2.capture(geForm);
    },
    (b3) => {
      const geForm = b3.tok({
        textOneOf: ['かわいげ', 'ありげ'],
      }, 'geForm');
      b3.capture(geForm);
    }
  );
});
