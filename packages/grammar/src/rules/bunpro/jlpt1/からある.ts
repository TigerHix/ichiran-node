import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: からある (karaaru) - As much as, As many as, At least
 *
 * A formal expression emphasizing a minimum substantial amount.
 * Used to indicate that something exists from (A) amount onwards.
 *
 * Structure: Number + Counter + からある/からいる/からの
 *
 * Examples:
 * - ６０キロからあるダンベル (dumbbells weighing as much as 60kg)
 * - ５０００枚からあるチケット (as many as 5000 tickets)
 * - ３０００人からいる会社 (a company with at least 3000 people)
 * - 一万人からの人数 (as many as 10000 people)
 *
 * Key discriminators:
 * - Follows number + counter (NUM + NOUN compounds in GiNZA)
 * - から is a particle (ADP/SCONJ) meaning "from"
 * - ある is the intransitive verb "to be/exist" (for inanimate objects)
 * - いる is the intransitive verb "to exist" (for living things)
 * - の is the nominalizer (pre-nominal form)
 *
 * Usage notes:
 * - からある: for inanimate objects (weights, distances, sizes)
 * - からいる: for living things (people, animals) - less common than の
 * - からの: pre-nominal form (more common than からいる for living things)
 *
 * GiNZA parse structure:
 * - NUM + NOUN(counter) + から(ADP/SCONJ) + ある/いる(AUX/VERB) or の(ADP/PART)
 *
 * Different from:
 * - からする (for prices/costs)
 * - 以上① (more general "X or more")
 * - から alone as "because" or "from"
 */
export default bunproLinguisticRule('からある', (r) => {
  r.either(
    // Pattern 1: Number + Counter + からある (inanimate objects)
    // GiNZA sometimes parses ある as DET when it's part of a quantifier compound
    (b1) => {
      const kara = b1.particle('から', 'kara');
      const aru = b1.tok({ lemma: 'ある', posOneOf: ['AUX', 'VERB', 'DET'] }, 'aru');
      b1.inOrder(kara, aru, 1);
      b1.captureSpan('からある', kara, aru);
    },

    // Pattern 2: Number + Counter + からいる (living things)
    (b2) => {
      const kara = b2.particle('から', 'kara');
      const iru = b2.tok({ lemma: 'いる', posOneOf: ['AUX', 'VERB', 'DET'] }, 'iru');
      b2.inOrder(kara, iru, 1);
      b2.captureSpan('からある', kara, iru);
    },

    // Pattern 3: Number + Counter + からの (pre-nominal form)
    (b3) => {
      const kara = b3.particle('から', 'kara');
      const no = b3.particle('の', 'no');
      b3.inOrder(kara, no, 1);
      b3.captureSpan('からある', kara, no);
    }
  );
});
