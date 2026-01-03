import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('あそこ', (r) => {
  // あそこ (asoko) - demonstrative pronoun meaning "over there"
  // Refers to a place far from both the speaker and the listener
  //
  // Matches the hiragana form あそこ.
  // GiNZA parses it as PRON with lemma=あそこ.

  const asoko = r.tok({ lemma: 'あそこ', pos: 'PRON' }, 'asoko');
  r.capture(asoko);
});
