import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: かと思ったら・かと思うと (ka to omottara / ka to omou to) - "just when, as soon as, no sooner than"
 *
 * A hypothetical structure showing that when an uncertain thought of (A) arose,
 * something conflicting with that thought occurred.
 *
 * Structures:
 * - Verb［past］+ かと思ったら / かと思うと
 * - Verb［past］+ 思ったら / 思うと (without か)
 * - Noun + かと思ったら / かと思うと
 *
 * GiNZA parse structure:
 * - Uses flexible text-based matching to handle variable tokenization
 */
export default linguisticRule('かと思ったら-かと思うと', (r) => {
  r.either(
    // Pattern 1: か + と + tokens containing おも/思
    (b) => {
      const ka = b.tok({ text: 'か' }, 'ka');
      const to = b.tok({ text: 'と' }, 'to');
      const omo = b.tok({
        textOneOf: ['おも', '思', 'おもう', '思う', 'おもった', '思った', 'おもったら', '思ったら'],
      }, 'omo');
      b.inOrder(ka, to, 5).inOrder(to, omo, 5);
      b.captureSpan('pattern', ka, omo);
    },

    // Pattern 2: と + tokens containing おも/思 (without か)
    (b) => {
      const to = b.tok({ text: 'と' }, 'to');
      const omo = b.tok({
        textOneOf: ['おも', '思', 'おもう', '思う', 'おもった', '思った', 'おもったら', '思ったら'],
      }, 'omo');
      b.inOrder(to, omo, 5);
      b.captureSpan('pattern', to, omo);
    },

    // Pattern 3: Single token forms
    (b) => {
      const pattern = b.tok({
        textOneOf: [
          'かとおもったら', 'かとおもうと',
          'とおもったら', 'とおもうと',
          'かと思ったら', 'かと思うと',
          'と思ったら', 'と思うと',
        ],
      }, 'pattern');
      b.capture(pattern);
    }
  );
});
