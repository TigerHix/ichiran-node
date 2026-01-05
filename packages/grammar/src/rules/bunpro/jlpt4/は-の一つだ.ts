import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('は-の一つだ', (r) => {
  // Pattern: Noun(A) + は/も + NounPhrase(B) + の + [counter] + だ/です
  // Meaning: "X is one of (many) Y"
  //
  // Examples:
  // - 電車は乗り物のひとつです (Trains are one of vehicles)
  // - キリスト教は宗教のひとつだ (Christianity is one of religions)
  // - 彼も家族のひとりだ (He is one of the family members)
  // - このクレヨンはこのセットのいっぽんだ (This crayon is one of this set)
  // - トマトはフルーツのいっしゅだ (Tomatoes are one type of fruit)
  //
  // Key discriminators:
  // - は/も as topic marker (dep=case)
  // - の as particle connecting category noun to counter (dep=case)
  // - Counter as NUM or NOUN (various counters: 一つ, ひとつ, 一人, ひとり, 一種, いっしゅ, etc.)
  // - だ/です as copula (AUX, lemma=だ or text=です)
  //
  // Negative cases to avoid (possessive "no", not counter):
  // - これは私の本です (This is my book - NOT "one of")
  // - 彼は日本の学生です (He is a Japanese student - NOT "one of")
  //
  // The counter varies based on what's being counted:
  // - 一つ/ひとつ - general counter (NUM)
  // - 一人/ひとり - people (NUM or NOUN)
  // - 一本/いっぽん - long objects (NUM or NOUN)
  // - 一種/いっしゅ - types/kinds (NUM or NOUN)
  //
  // Key discriminator: Counter must be NUM (most reliable way to distinguish from regular nouns)
  //
  // Note: GiNZA tokenizes "ひとつ" as "ひと" + "つ", where "ひと" is NUM

  r.either(
    // Pattern 1: は topic marker
    (b) => {
      const wa = b.particle('は', 'wa');
      const no = b.particle('の', 'no');
      // Counter must be NUM to avoid possessive constructions like "私の本"
      const counter = b.tok({ pos: 'NUM' }, 'counter');

      b.either(
        // Pattern 1a: Casual - だ (AUX with lemma=だ)
        (b2) => {
          const da = b2.aux({ lemma: 'だ' }, 'da');
          // wa < ... < no < counter < (1-2 tokens) < da
          b2.inOrder(wa, no).inOrder(no, counter, 1).inOrder(counter, da, 3);
          b2.captureSpan('は-の一つだ', wa, da);
        },
        // Pattern 1b: Polite - です (AUX with text=です)
        (b2) => {
          const desu = b2.aux({ text: 'です' }, 'desu');
          b2.inOrder(wa, no).inOrder(no, counter, 1).inOrder(counter, desu, 3);
          b2.captureSpan('は-の一つだ', wa, desu);
        }
      );
    },
    // Pattern 2: も topic marker (also, too)
    (b) => {
      const mo = b.particle('も', 'mo');
      const no = b.particle('の', 'no');
      const counter = b.tok({ pos: 'NUM' }, 'counter');

      b.either(
        // Pattern 2a: Casual - だ (AUX with lemma=だ)
        (b2) => {
          const da = b2.aux({ lemma: 'だ' }, 'da');
          b2.inOrder(mo, no).inOrder(no, counter, 1).inOrder(counter, da, 3);
          b2.captureSpan('は-の一つだ', mo, da);
        },
        // Pattern 2b: Polite - です (AUX with text=です)
        (b2) => {
          const desu = b2.aux({ text: 'です' }, 'desu');
          b2.inOrder(mo, no).inOrder(no, counter, 1).inOrder(counter, desu, 3);
          b2.captureSpan('は-の一つだ', mo, desu);
        }
      );
    }
  );
});
