import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('これ', (r) => {
  // Demonstrative pronoun "kore" - this one (thing near speaker)
  // Can be written as hiragana これ or kanji 此れ (rare)
  r.either(
    // Hiragana form これ
    (b) => {
      const kore = b.tok({ lemma: 'これ', posOneOf: ['PRON', 'NOUN'] }, 'kore');
      b.capture(kore);
    },
    // Kanji form 此れ (rare but noted in Bunpro data)
    (b) => {
      const koreKanji = b.tok({ text: '此れ', posOneOf: ['PRON', 'NOUN'] }, 'kore');
      b.capture(koreKanji);
    }
  );
});
