import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('それ', (r) => {
  // Demonstrative pronoun "sore" - that one (thing near listener, far from speaker)
  // Can be written as hiragana それ or kanji 其れ (rare)
  r.either(
    // Hiragana form それ
    (b) => {
      const sore = b.tok({ lemma: 'それ', posOneOf: ['PRON', 'NOUN', 'PROPN'] }, 'sore');
      b.capture(sore);
    },
    // Kanji form 其れ (rare but noted in Bunpro data)
    (b) => {
      const soreKanji = b.tok({ text: '其れ', posOneOf: ['PRON', 'NOUN', 'PROPN'] }, 'sore');
      b.capture(soreKanji);
    }
  );
});
