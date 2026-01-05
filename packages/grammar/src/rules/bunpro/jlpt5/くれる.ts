import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('くれる', (r) => {
  // くれる (kureru) - to give (to me/speaker)
  // Match both hiragana and kanji forms
  r.either(
    // Hiragana form: くれる
    (b) => {
      const kureru = b.verb({ lemma: 'くれる' }, 'kureru');
      b.capture(kureru);
    },
    // Kanji form: 呉れる
    (b) => {
      const kureru = b.verb({ lemma: '呉れる' }, 'kureru');
      b.capture(kureru);
    }
  );
});
