import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('くらい1', (r) => {
  // くらい/ぐらい as adverbial particle indicating approximate amount or degree
  // Follows quantities, numbers, counters, or question words
  r.either(
    (b) => {
      const kurai = b.particle('くらい', 'kurai');
      b.capture(kurai);
    },
    (b) => {
      const gurai = b.particle('ぐらい', 'gurai');
      b.capture(gurai);
    }
  );
});
