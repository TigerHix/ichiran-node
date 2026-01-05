import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('誰か-どこか-誰も-どこも', (r) => {
  // 誰か・どこか・誰も・どこも (dareka/dokoka/daremo/dokomo): someone, somewhere, not anyone, not anywhere
  // Indefinite pronouns for uncertain people or places
  // Pattern: WH-Word (どこ、誰) + か/も + Particle

  r.either(
    // Pattern 1: どこか, 誰か (dareka, dokoka) - someone, somewhere (positive)
    // GiNZA parses か as either ADP/助詞-副助詞 or ADP/助詞-終助詞
    (r1) => {
      const whWord = r1.tok({
        pos: 'PRON',
        textOneOf: ['誰', 'どこ', 'だれ'],
      }, 'whWord');
      const ka = r1.tok({
        text: 'か',
        pos: 'ADP',
        tagOneOf: ['助詞-副助詞', '助詞-終助詞'],
      }, 'ka');

      r1.inOrder(whWord, ka, 1);
      r1.captureSpan('誰か-どこか-誰も-どこも', whWord, ka);
    },

    // Pattern 2: どこにも, 誰にも (dokoni, darenimo) - nowhere, no one (negative)
    (r2) => {
      const whWord = r2.tok({
        pos: 'PRON',
        textOneOf: ['誰', 'どこ', 'だれ'],
      }, 'whWord');
      const particle = r2.tok({ pos: 'ADP', dep: 'case' }, 'particle'); // に, へ, etc.
      const mo = r2.tok({
        text: 'も',
        tag: '助詞-係助詞',
      }, 'mo');

      r2.inOrder(whWord, particle, 1);
      r2.inOrder(particle, mo, 1);
      r2.captureSpan('誰か-どこか-誰も-どこも', whWord, mo);
    },

    // Pattern 3: どこも, 誰も (dokomo, daremo) - always, everywhere / no one, nowhere
    // GiNZA parses も as either PART or ADP
    (r3) => {
      const whWord = r3.tok({
        pos: 'PRON',
        textOneOf: ['誰', 'どこ', 'だれ'],
      }, 'whWord');
      const mo = r3.tok({
        text: 'も',
        tag: '助詞-係助詞',
      }, 'mo');

      r3.inOrder(whWord, mo, 1);
      r3.captureSpan('誰か-どこか-誰も-どこも', whWord, mo);
    }
  );
});
