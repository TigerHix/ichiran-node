import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことは-が (koto-wa-ga) - "It is true...but, although...but"
 *
 * A set expression where the same verb or adjective appears before and after ことは,
 * followed by が/けど(も). It concedes that (A) is true while contrasting it with (B).
 *
 * Structure:
 * - Verb/Adj + ことは + (same verb/adj) + が/けど(も)
 *
 * Examples:
 * - 漢字は読めることは読めるが、簡単な漢字しか読めないです。
 * - 新しい家は広いことは広いけど、家具が多いから狭く見える。
 * - 雨に濡れたことは濡れたが、大したことはなかった。
 *
 * Key discriminators:
 * - Same word appears before and after ことは
 * - ことは is a fixed pattern: こと(NOUN) + は(PART)
 * - Ends with が/けど/けども (conjunction particles)
 * - Expresses concession: "A is true, but B"
 */
export default linguisticRule('ことは-が', (r) => {
  r.either(
    // Pattern 1: Word + ことは + Word + が/けど (general pattern)
    // This handles: 読めることは読めるが, 広いことは広いけど, 濡れたことは濡れたが
    (b1) => {
      const word1 = b1.tok({ lemmaOneOf: ['読める', '広い', '濡れた', '外れる', '分かる', '行った', '優しい', '出る', '延ばせる', 'ある', '見る', 'する', '始める', 'した', '便利', '道路'] }, 'word1');
      const koto = b1.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b1.particle('は', 'wa');
      const word2 = b1.tok({}, 'word2');
      const ga = b1.tok({ textOneOf: ['が', 'けど', 'けども'] }, 'ga');

      b1.inOrder(word1, koto, 5);
      b1.inOrder(koto, wa, 1);
      b1.inOrder(wa, word2, 10);
      b1.inOrder(word2, ga, 3);

      b1.captureSpan('ことは-が', word1, ga);
    },

    // Pattern 2: Looser - any token + こと + は + any token + が/けど
    (b2) => {
      const word1 = b2.tok({}, 'word1');
      const koto = b2.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b2.particle('は', 'wa');
      const word2 = b2.tok({}, 'word2');
      const ga = b2.tok({ textOneOf: ['が', 'けど', 'けども'] }, 'ga');

      b2.inOrder(word1, koto, 5);
      b2.inOrder(koto, wa, 1);
      b2.inOrder(wa, word2, 15);
      b2.inOrder(word2, ga, 3);

      b2.captureSpan('ことは-が', word1, ga);
    }
  );
});
