import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('という-called', (r) => {
  // Pattern: Noun/Phrase + という + Noun
  // Meaning: "called X", "named X", "known as X"
  // Examples: ポケモンというゲーム (a game called Pokemon), 佐藤浩一という人 (a person named Sato Koichi)
  //
  // This grammar point connects two nouns to indicate that B is called/known as A.
  // - A (before という) can be: proper noun, common noun, quoted phrase
  // - B (after いう) is the category: ゲーム (game), 人 (person), 職業 (job), etc.
  //
  // Key discriminators from similar patterns:
  // - Different from かというと (question + "if we ask") - requires question particle か
  // - Different from というのは (topic marker + nominalizer) - followed by は
  // - Different from ということ (nominalizer) - followed by こと
  //
  // GiNZA parse structure for "ポケモンというゲーム":
  // - ポケモン (PROPN/NOUN)
  // - と (ADP, dep=case) --case--> ポケモン
  // - いう (VERB, dep=compound) --compound--> ポケモン (head points back to first noun)
  // - ゲーム (NOUN) - follows いう
  //
  // The key constraint: いう must be followed by a noun/proper noun/pronoun
  // (not by particles like は, も, etc.)

  const to = r.particle('と', 'to');
  const iu = r.verb({ lemma: 'いう' }, 'iu');

  r.inOrder(to, iu, 1);

  // いう is followed by a noun (B) or auxiliary/quote marker
  // The noun can have various deps (flat, nmod, etc.) depending on sentence structure
  r.either(
    // Pattern 1: Followed by regular noun
    (b) => {
      const nextNoun = b.noun({});
      b.inOrder(iu, nextNoun, 1);
      b.captureSpan('という', to, iu);
    },
    // Pattern 2: Followed by proper noun
    (b) => {
      const nextNoun = b.tok({ pos: 'PROPN' });
      b.inOrder(iu, nextNoun, 1);
      b.captureSpan('という', to, iu);
    },
    // Pattern 3: Followed by pronoun
    (b) => {
      const nextNoun = b.tok({ pos: 'PRON' });
      b.inOrder(iu, nextNoun, 1);
      b.captureSpan('という', to, iu);
    },
    // Pattern 4: Followed by ん (nominalizer/quote marker in casual speech)
    // Example: 強いというんだけど (he's said to be strong, but...)
    (b) => {
      const n = b.tok({ text: 'ん' });
      b.inOrder(iu, n, 1);
      b.captureSpan('という', to, iu);
    },
    // Pattern 5: Followed by の (nominalizer variant)
    (b) => {
      const no = b.tok({ text: 'の' });
      b.inOrder(iu, no, 1);
      b.captureSpan('という', to, iu);
    }
  );
});
