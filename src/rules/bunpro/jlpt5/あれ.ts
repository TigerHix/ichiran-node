import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('あれ', (r) => {
  // Demonstrative pronoun "are" - that one over there (thing far from both speaker and listener)
  // Can be written as hiragana あれ or kanji 彼れ (rare)
  r.either(
    // Hiragana form あれ
    (b) => {
      const are = b.tok({ lemma: 'あれ', posOneOf: ['PRON', 'NOUN'] }, 'are');
      b.capture(are);
    },
    // Kanji form 彼れ (rare but noted in Bunpro data)
    (b) => {
      const areKanji = b.tok({ text: '彼れ', posOneOf: ['PRON', 'NOUN'] }, 'are');
      b.capture(areKanji);
    }
  );
});
