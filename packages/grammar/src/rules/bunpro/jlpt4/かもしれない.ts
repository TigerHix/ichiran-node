import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('かもしれない', (r) => {
  // かもしれない/かもしれません expresses uncertainty ("might be", "may be")
  // Pattern: plain form + かもしれない/かもしれません
  //
  // GiNZA parses this as:
  // - か (PART, dep=mark)
  // - も (ADP, dep=fixed)
  // - しれ (VERB, lemma=しれる, dep=fixed)
  // - ない (AUX, lemma=ない, dep=fixed)
  //
  // Forms:
  // - かもしれない (plain)
  // - かもしれません (polite)
  // - かもしれん (colloquial, less common)
  // - かも (shortened colloquial)

  r.either(
    // Pattern 1: かもしれない (plain form)
    (b) => {
      const ka = b.particle('か', 'ka');
      const mo = b.tok({ text: 'も', dep: 'fixed' }, 'mo');
      const shire = b.tok({ lemma: 'しれる', dep: 'fixed' }, 'shire');
      const nai = b.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');

      b.inOrder(ka, mo, 1);
      b.inOrder(mo, shire, 1);
      b.inOrder(shire, nai, 1);

      b.captureSpan('かもしれない', ka, nai);
    },

    // Pattern 2: かもしれません (polite form)
    (b) => {
      const ka = b.particle('か', 'ka');
      const mo = b.tok({ text: 'も', dep: 'fixed' }, 'mo');
      const shire = b.tok({ lemma: 'しれる', dep: 'fixed' }, 'shire');
      const mase = b.tok({ text: 'ませ', dep: 'fixed' }, 'mase');
      const n = b.aux({ text: 'ん', dep: 'fixed' }, 'n');

      b.inOrder(ka, mo, 1);
      b.inOrder(mo, shire, 1);
      b.inOrder(shire, mase, 1);
      b.inOrder(mase, n, 1);

      b.captureSpan('かもしれません', ka, n);
    },

    // Pattern 3: かもしれん (colloquial)
    (b) => {
      const ka = b.particle('か', 'ka');
      const mo = b.tok({ text: 'も', dep: 'fixed' }, 'mo');
      const shire = b.tok({ lemma: 'しれる', dep: 'fixed' }, 'shire');
      const n = b.aux({ text: 'ん', dep: 'fixed' }, 'n');

      b.inOrder(ka, mo, 1);
      b.inOrder(mo, shire, 1);
      b.inOrder(shire, n, 1);

      b.captureSpan('かもしれん', ka, n);
    },

    // Pattern 4: かも (shortened colloquial form)
    // GiNZA parses this as:
    // - か (PART, dep=mark)
    // - も (ADP, dep=case) - different from full form!
    (b) => {
      const ka = b.particle('か', 'ka');
      const mo = b.particle('も', 'mo');
      b.inOrder(ka, mo, 1);
      b.captureSpan('かも', ka, mo);
    }
  );
});
