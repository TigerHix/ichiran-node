import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('どこ', (r) => {
  // どこ (doko) - interrogative pronoun meaning "where"
  // Refers to an unknown place/location
  //
  // Matches both hiragana (どこ) and kanji (何処) forms.
  // GiNZA parses both as PRON with their respective lemmas.

  r.either(
    // Branch 1: Hiragana form どこ (lemma=どこ)
    (branch) => {
      const doko = branch.tok({ lemma: 'どこ', posOneOf: ['PRON', 'NOUN', 'PROPN', 'ADV'] }, 'doko');
      branch.capture(doko);
    },
    // Branch 2: Kanji form 何処 (lemma=何処)
    (branch) => {
      const doko = branch.tok({ lemma: '何処', posOneOf: ['PRON', 'NOUN', 'PROPN', 'ADV'] }, 'doko');
      branch.capture(doko);
    }
  );
});
