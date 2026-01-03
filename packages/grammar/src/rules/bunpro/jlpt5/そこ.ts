import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('そこ', (r) => {
  // そこ (soko) - demonstrative pronoun meaning "there" or "that place"
  // Refers to a place near the listener (or previously mentioned place)
  //
  // Matches both hiragana (そこ) and kanji (其処) forms.
  // GiNZA parses both as PRON with their respective lemmas.
  // Note: The lemma for 其処 is "其処", not "そこ"

  r.either(
    // Branch 1: Hiragana form そこ (lemma=そこ)
    (branch) => {
      const soko = branch.tok({ lemma: 'そこ', pos: 'PRON' }, 'soko');
      branch.capture(soko);
    },
    // Branch 2: Kanji form 其処 (lemma=其処)
    (branch) => {
      const soko = branch.tok({ lemma: '其処', pos: 'PRON' }, 'soko');
      branch.capture(soko);
    }
  );
});
