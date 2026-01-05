import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ここ', (r) => {
  // ここ (koko) - demonstrative pronoun meaning "here" or "this place"
  // Refers to a place near the speaker
  //
  // Matches both hiragana (ここ) and kanji (此処) forms.
  // GiNZA parses both as PRON with their respective lemmas.
  // Note: The lemma for 此処 is "此処", not "ここ"

  r.either(
    // Branch 1: Hiragana form ここ (lemma=ここ)
    (branch) => {
      const koko = branch.tok({ lemma: 'ここ', pos: 'PRON' }, 'koko');
      branch.capture(koko);
    },
    // Branch 2: Kanji form 此処 (lemma=此処)
    (branch) => {
      const koko = branch.tok({ lemma: '此処', pos: 'PRON' }, 'koko');
      branch.capture(koko);
    }
  );
});
