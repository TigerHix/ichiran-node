import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ように・ような - As/like, Just like
 *
 * Matches patterns where ように/ような follow a phrase to express similarity
 * or manner "like X" or "as if X".
 *
 * Structure:
 * - Verb/Adj + ように (adverbial form) → modifies verbs/adjectives
 * - Verb/Adj + ような (attributive form) → modifies nouns
 * - Noun + の + ように/ような (also covered here)
 *
 * Examples:
 * - 彼は亀のように走る (He runs like a turtle)
 * - 食べすぎたような顔をしている (Has a face like he ate too much)
 * - 想像できないようなアイディア (Ideas like you can't imagine)
 *
 * Key discriminators:
 * - よう is a na-adjective meaning "appearance/manner"
 * - Conjugates with auxiliary だ:
 *   - だ → に (adverbial/連用形-ニ) for ように
 *   - だ → な (attributive/連体形-一般) for ような
 * - Can follow verbs, adjectives, or noun+の
 *
 * Note: This covers both verb/adj + ように/ような AND noun + の + ように/ような.
 * The test data includes both patterns.
 */
export default linguisticRule('ように-ような', (r) => {
  r.either(
    // Pattern 1: ように (adverbial - modifies verbs/adjectives)
    (b) => {
      // Find よう token by text (lemma varies in GiNZA parsing)
      const you = b.tok({ text: 'よう' }, 'you');

      // に (adverbial particle from だ → 連用形-ニ)
      const ni = b.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');

      // Order: you -> ni (contiguous)
      b.inOrder(you, ni, 1);

      // Find any preceding token (within reasonable distance)
      const prev = b.tok({}, 'prev');
      b.inOrder(prev, you, 5);

      b.captureSpan('ように-ような', prev, ni);
    },
    // Pattern 2: ような (attributive - modifies nouns)
    (b) => {
      // Find よう token by text
      const you = b.tok({ text: 'よう' }, 'you');

      // な (attributive particle from だ → 連体形-一般)
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');

      // Order: you -> na (contiguous)
      b.inOrder(you, na, 1);

      // Find any preceding token (within reasonable distance)
      const prev = b.tok({}, 'prev');
      b.inOrder(prev, you, 5);

      b.captureSpan('ように-ような', prev, na);
    },
    // Pattern 3: Single token "ように" (GiNZA sometimes parses as one token)
    (b) => {
      const yoni = b.tok({ text: 'ように' }, 'yoni');

      // Find any preceding token
      const prev = b.tok({}, 'prev');
      b.inOrder(prev, yoni, 5);

      b.captureSpan('ように-ような', prev, yoni);
    },
    // Pattern 4: Single token "ような" (GiNZA sometimes parses as one token)
    (b) => {
      const yona = b.tok({ text: 'ような' }, 'yona');

      // Find any preceding token
      const prev = b.tok({}, 'prev');
      b.inOrder(prev, yona, 5);

      b.captureSpan('ように-ような', prev, yona);
    }
  );
});
