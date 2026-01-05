import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('だれ', (r) => {
  // だれ (dare) - interrogative pronoun meaning "who"
  // Refers to an unknown person
  //
  // Matches both hiragana (だれ) and kanji (誰) forms.
  // GiNZA parses both as PRON with their respective lemmas.
  //
  // The polite variant どなた (donata) is also supported.
  //
  // Examples:
  // - 誰が来ますか (Who is coming?)
  // - 誰ですか (Who is it?)
  // - 誰の本ですか (Whose book is it?)
  // - 誰かがいる (Someone is here)
  // - 誰もいない (Nobody is here)

  r.either(
    // Branch 1: Hiragana form だれ (lemma=だれ)
    (branch) => {
      const dare = branch.tok({ lemma: 'だれ', pos: 'PRON' }, 'dare');
      branch.capture(dare);
    },
    // Branch 2: Kanji form 誰 (lemma=誰)
    (branch) => {
      const dare = branch.tok({ lemma: '誰', pos: 'PRON' }, 'dare');
      branch.capture(dare);
    },
    // Branch 3: Polite form どなた (lemma=どなた)
    (branch) => {
      const dare = branch.tok({ lemma: 'どなた', pos: 'PRON' }, 'dare');
      branch.capture(dare);
    }
  );
});
