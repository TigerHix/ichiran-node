import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ほかにも-ほかには (他/ほか + particles) - Other than, Besides, In addition, Another
 *
 * Matches patterns using "他/ほか" (other/another) with various particles:
 * - ほかにも (also/besides) - 他/ほか + に + も
 * - ほかには (other than) - 他/ほか + に + は (typically negative sentences)
 * - ほかに (another/other) - 他/ほか + に
 * - ほかの (another/other) - 他/ほか + の (adjectival use before noun)
 * - ほかは (other things) - 他/ほか + は
 *
 * Structure:
 * - 他/ほか (NOUN: other/another)
 * - Followed by particles: に, も, は, の
 *
 * Key discriminators:
 * - 他/ほか must be NOUN with lemma=他 or text=他/ほか
 * - Particles must have dep=case (particle attachment)
 * - This distinguishes from locative 外 (outside) and similar homonyms
 */
export default bunproLinguisticRule('ほかにも-ほかには', (r) => {
  r.either(
    // Pattern 1: ほかにも (他/ほか + に + も) - "also/besides/anything else"
    (b) => {
      const hoka_nimo = b.tok({ textOneOf: ['他', 'ほか'] }, 'hoka');
      const ni_nimo = b.particle('に', 'ni');
      const mo = b.particle('も', 'mo');
      b.inOrder(hoka_nimo, ni_nimo, 1);
      b.inOrder(ni_nimo, mo, 1);
      b.captureSpan('ほかにも', hoka_nimo, mo);
    },
    // Pattern 2: ほかには (他/ほか + に + は) - "other than" (typically negative)
    (b) => {
      const hoka_niwa = b.tok({ textOneOf: ['他', 'ほか'] }, 'hoka');
      const ni_niwa = b.particle('に', 'ni');
      const wa_niwa = b.particle('は', 'wa');
      b.inOrder(hoka_niwa, ni_niwa, 1);
      b.inOrder(ni_niwa, wa_niwa, 1);
      b.captureSpan('ほかには', hoka_niwa, wa_niwa);
    },
    // Pattern 3: ほかに (他/ほか + に) - "another/other" (basic form)
    (b) => {
      const hoka_ni = b.tok({ textOneOf: ['他', 'ほか'] }, 'hoka');
      const ni_ni = b.particle('に', 'ni');
      b.inOrder(hoka_ni, ni_ni, 1);
      b.captureSpan('ほかに', hoka_ni, ni_ni);
    },
    // Pattern 4: ほかの (他/ほか + の) - "another/other" (adjectival)
    (b) => {
      const hoka_no = b.tok({ textOneOf: ['他', 'ほか'] }, 'hoka');
      const no = b.particle('の', 'no');
      b.inOrder(hoka_no, no, 1);
      b.captureSpan('ほかの', hoka_no, no);
    },
    // Pattern 5: ほかは (他/ほか + は) - "other things" (topic marker)
    (b) => {
      const hoka_wa = b.tok({ textOneOf: ['他', 'ほか'] }, 'hoka');
      const wa_wa = b.particle('は', 'wa');
      b.inOrder(hoka_wa, wa_wa, 1);
      b.captureSpan('ほかは', hoka_wa, wa_wa);
    }
  );
});
