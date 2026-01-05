import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('あくまでも', (r) => {
  // あくまでも (akumademo) - adverb meaning "thoroughly, absolutely, to the bitter end, persistently, stubbornly"
  // Can be written as あくまでも (hiragana) or 飽くまでも (kanji)
  // Short form あくまで / 飽くまで is also accepted
  //
  // Pattern: adverb あくまでも (can be written as 飽くまで)
  // Modifies verbs and adjectives
  //
  // GiNZA parses あくまでも as a single ADV token
  // The も particle is optional (あくまで or あくまでも)

  r.either(
    // Full form: あくまでも / 飽くまでも (with も)
    (b) => {
      const akumademo = b.adv({
        textOneOf: ['あくまでも', '飽くまでも'],
      }, 'akumademo');
      b.capture(akumademo);
    },
    // Short form: あくまで / 飽くまで (without も)
    (b) => {
      const akumade = b.adv({
        textOneOf: ['あくまで', '飽くまで'],
      }, 'akumade');
      b.capture(akumade);
    }
  );
});
