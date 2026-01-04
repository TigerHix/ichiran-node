import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('以上1', (r) => {
  // 以上① (ijou1) - noun suffix meaning "more than or equal to", "over", "greater than"
  // Can also mean "since" or "now that" when used with verbs/sentences
  //
  // This grammar point indicates a minimum limit - "X or more", "at least X".
  // The antonym of いか (ika).
  //
  // Examples:
  // - １００人以上 (100 people or more)
  // - ３００日以上トレーニングが出来た (was able to train for more than 300 days)
  // - これ以上運動をしたくない (I don't want to exercise any more than this)
  // - 以上です (that's all / I'm done)
  //
  // GiNZA parses 以上 as a NOUN (noun suffix) in all contexts.
  // The test data includes both kanji (以上) and hiragana (いじょう) forms.
  // - Kanji 以上 has lemma='いじょう'
  // - Hiragana いじょう may have different POS/lemma depending on context
  //
  // We need to match:
  // 1. Number/Amount + 以上
  // 2. Pronoun + 以上 (これ, それ, あれ)
  // 3. Standalone 以上 at sentence end (以上です)

  r.either(
    // Branch 1: Match by text for hiragana form (e.g., いじょう, いじょうは)
    (b1) => {
      const ijou = b1.tok({ textOneOf: ['いじょう', '以上'] }, 'ijou');
      b1.capture(ijou);
    },
    // Branch 2: Number + counter + 以上 in kanji (e.g., １００人以上, ３００日以上)
    (b2) => {
      const ijou = b2.noun({ text: '以上', lemma: 'いじょう' }, 'ijou');
      // Follows a number + counter (complex NUM + NOUN compound in GiNZA)
      // We just need to ensure 以上 comes after some token
      const prev = b2.tok({}, 'prev');
      b2.inOrder(prev, ijou, 3); // Within 3 tokens
      b2.capture(ijou);
    },
    // Branch 3: Pronoun/Demonstrative + 以上 in kanji (e.g., これ以上, それ以上)
    (b3) => {
      const ijou = b3.noun({ text: '以上', lemma: 'いじょう' }, 'ijou');
      const pronoun = b3.tok({ posOneOf: ['PRON', 'NOUN'] }, 'pronoun');
      b3.inOrder(pronoun, ijou, 1);
      b3.capture(ijou);
    },
    // Branch 4: Standalone 以上 in kanji at sentence end (e.g., 以上です)
    (b4) => {
      const ijou = b4.noun({ text: '以上', lemma: 'いじょう' }, 'ijou');
      // No preceding constraint - just match 以上
      b4.capture(ijou);
    }
  );
});
