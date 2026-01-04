import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('誰か-どこか-誰も-どこも', (r) => {
  // 誰か・どこか・誰も・どこも (dareka/dokoka/daremo/dokomo): someone, somewhere, not anyone, not anywhere
  // Indefinite pronouns for uncertain people or places
  // Pattern: WH-Word (どこ、誰) + か/も + Particle

  r.either(
    // Pattern 1: どこか, 誰か (dareka, dokoka) - someone, somewhere (positive)
    (r1) => {
      const whWord = r1.tok({
        pos: 'PRON',
        textOneOf: ['誰', 'どこ', 'だれ'],
      }, 'whWord');
      const ka = r1.particle('か', 'ka', { pos: 'PART' });

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
      const mo = r2.particle('も', 'mo', { pos: 'PART' });

      r2.inOrder(whWord, particle, 1);
      r2.inOrder(particle, mo, 1);
      r2.captureSpan('誰か-どこか-誰も-どこも', whWord, mo);
    },

    // Pattern 3: どこも, 誰も (dokomo, daremo) - always, everywhere / no one, nowhere
    (r3) => {
      const whWord = r3.tok({
        pos: 'PRON',
        textOneOf: ['誰', 'どこ', 'だれ'],
      }, 'whWord');
      const mo = r3.particle('も', 'mo', { pos: 'PART' });

      r3.inOrder(whWord, mo, 1);
      r3.captureSpan('誰か-どこか-誰も-どこも', whWord, mo);
    }
  );
});
