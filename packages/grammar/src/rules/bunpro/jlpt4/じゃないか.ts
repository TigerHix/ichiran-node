import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('じゃないか', (r) => {
  // じゃないか (isn't it? / right?) - Casual negated question seeking agreement
  // Variants: じゃないか, ではないか, じゃないですか, ではないですか
  // Attaches to: verbs, i-adjectives, nouns, na-adjectives
  // Key discriminators: sentence-final particle か, aux ない

  r.either(
    // Pattern 1: Standard form じゃないか
    (branch1) => {
      const ja = branch1.aux({ text: 'じゃ', lemma: 'だ' }, 'ja');
      const nai = branch1.aux({ lemma: 'ない' }, 'nai');
      const ka = branch1.tok({ text: 'か', depOneOf: ['mark', 'root'] }, 'ka');

      // Consecutive: じゃ + ない + か
      branch1.inOrder(ja, nai, 1).inOrder(nai, ka, 1);
      branch1.captureSpan('じゃないか', ja, ka);
    },
    // Pattern 1b: Formal ではないか
    (branch1b) => {
      // では is often parsed as ADP with lemma=で
      const de = branch1b.tok({ text: 'で' }, 'de');
      // nai might have different lemmas or POS in some parses
      const nai = branch1b.tok({ text: 'ない' }, 'nai');
      const ka = branch1b.tok({ text: 'か', depOneOf: ['mark', 'root'] }, 'ka');

      // Allow up to 2 tokens between de and nai (for では+nai parsing)
      branch1b.inOrder(de, nai, 2).inOrder(nai, ka, 1);
      branch1b.captureSpan('じゃないか', de, ka);
    },
    // Pattern 2: Polite form じゃないですか / ではないですか
    (branch2) => {
      const ja = branch2.aux({ textOneOf: ['じゃ', 'では'], lemma: 'だ' }, 'ja');
      const nai = branch2.aux({ lemma: 'ない' }, 'nai');
      const desu = branch2.aux({ lemma: 'です' }, 'desu');
      const ka = branch2.tok({ text: 'か', depOneOf: ['mark', 'root'] }, 'ka');

      // じゃ + ない + です + か
      branch2.inOrder(ja, nai, 1).inOrder(nai, desu, 1).inOrder(desu, ka, 1);
      branch2.captureSpan('じゃないか', ja, ka);
    },
    // Pattern 3: Past tense じゃなかったか / ではなかったか
    (branch3) => {
      const ja = branch3.aux({ textOneOf: ['じゃ', 'では'], lemma: 'だ' }, 'ja');
      // Past form of ない is なかった
      const nakatta = branch3.aux({ text: 'なかっ' }, 'nakatta');
      const ta = branch3.aux({ lemma: 'た' }, 'ta');
      const ka = branch3.tok({ text: 'か', depOneOf: ['mark', 'root'] }, 'ka');

      // じゃ + なかっ + た + か
      branch3.inOrder(ja, nakatta, 1).inOrder(nakatta, ta, 1).inOrder(ta, ka, 1);
      branch3.captureSpan('じゃないか', ja, ka);
    }
  );
});
