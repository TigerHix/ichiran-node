import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ておく', (r) => {
  // ておく (verb-te-form + おく): "to do something in advance"
  // Indicates doing something now for future benefit or to prevent something.
  //
  // Structure: Verb[て-form] + おく
  // Variants: Verb[て-form] + とく (casual contraction)
  //           Verb[で-form] + どく (casual contraction after で)
  //
  // GiNZA parses these similarly to てしまう:
  //   - おく as VERB with dep=fixed (when it's part of the V-te+oku construction)
  //   - とく/どく as AUX with dep=aux (contracted forms)
  //
  // Examples:
  //   予約しておく: おく[VERB, dep=fixed]
  //   汲んでとく: とく[AUX, dep=aux]
  //
  // We use r.either() to handle each parsing pattern:

  r.either(
    // Pattern 1: standard おく (dep=fixed, attached to te-form verb)
    (b) => {
      const oku = b.tok({ lemmaOneOf: ['おく', '置く'], dep: 'fixed' }, 'oku');
      b.capture(oku);
    },

    // Pattern 2: とく contraction (AUX, dep=aux after て)
    (b) => {
      const toku = b.tok({
        text: 'とく',
        posOneOf: ['AUX', 'VERB']
      }, 'toku');
      b.capture(toku);
    },

    // Pattern 3: どく contraction (AUX, dep=aux after で)
    (b) => {
      const doku = b.tok({
        text: 'どく',
        posOneOf: ['AUX', 'VERB']
      }, 'doku');
      b.capture(doku);
    }
  );
});
