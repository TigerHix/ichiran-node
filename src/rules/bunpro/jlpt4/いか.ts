import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いか', (r) => {
  // いか (ika) - noun suffix meaning "less than or equal to", "under", "below"
  // Written as 以下 in kanji. Attach to: Number + いか, Noun/Pronoun + いか
  //
  // This grammar point indicates a maximum limit - "X or less", "not exceeding X".
  // The antonym of 以上① (ijou).
  //
  // Examples:
  // - １００人以下 (100 people or less)
  // - ３０万円以下だった (was under 300,000 yen)
  // - これ以下の (this or less - adjective usage)
  // - 以下同文 (ditto - standalone usage)
  //
  // GiNZA parses 以下 as a NOUN (noun suffix) in all contexts.
  // The test data includes both kanji (以下) and hiragana (いか) forms.
  // - Kanji 以下 has lemma='いか'
  // - Hiragana いか may have different POS/lemma depending on context
  //
  // We need to match:
  // 1. Number/Amount + いか
  // 2. Pronoun + いか
  // 3. Standalone いか (at sentence beginning)

  r.either(
    // Branch 1: Match by text for hiragana form (e.g., ８０点いかなら)
    (b1) => {
      const ika = b1.tok({ text: 'いか' }, 'ika');
      b1.capture(ika);
    },
    // Branch 2: Number + counter + いか in kanji (e.g., １００人以下, ３０万円以下)
    (b2) => {
      const ika = b2.noun({ text: '以下', lemma: 'いか' }, 'ika');
      // Follows a number + counter (complex NUM + NOUN compound in GiNZA)
      // We just need to ensure いか comes after some token
      const prev = b2.tok({}, 'prev');
      b2.inOrder(prev, ika, 3); // Within 3 tokens
      b2.capture(ika);
    },
    // Branch 3: Pronoun/Demonstrative + いか in kanji (e.g., これ以下, それ以下)
    (b3) => {
      const ika = b3.noun({ text: '以下', lemma: 'いか' }, 'ika');
      const pronoun = b3.tok({ posOneOf: ['PRON', 'NOUN'] }, 'pronoun');
      b3.inOrder(pronoun, ika, 1);
      b3.capture(ika);
    },
    // Branch 4: Standalone いか in kanji at sentence start (e.g., 以下同文, 以下のもの)
    (b4) => {
      const ika = b4.noun({ text: '以下', lemma: 'いか' }, 'ika');
      // No preceding constraint - just match いか
      b4.capture(ika);
    }
  );
});
