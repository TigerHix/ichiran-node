import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('のだろうか', (r) => {
  // のだろうか/んだろうか/だろうか/んでしょうか/でしょうか expresses wondering/uncertainty ("I wonder if...")
  // Pattern: (plain form) + (の/ん) + (な) + だろう/でしょう + か
  //
  // GiNZA parses this as:
  // - With の/ん: の (SCONJ, dep=mark) or ん rarely appears
  // - With な (for nouns/na-adj): な (AUX, lemma=だ, dep=cop)
  // - だろう/でしょう (AUX, lemma=だ, inflectionForm=意志推量形)
  //   - dep=fixed when following の
  //   - dep=aux/cop when directly following verb/noun (no の)
  // - か (PART, dep=mark)
  //
  // The の/ん nominalizer is often optional (old-fashioned), creating multiple forms:
  // - 勝てるのだろうか (with nominalizer)
  // - 勝てるだろうか (without nominalizer)
  // - 食べ物なのだろうか (noun + な + の)
  // - 食べ物だろうか (noun, no nominalizer)
  //
  // And polite forms with でしょう instead of だろう.

  r.either(
    // Pattern 1: (の/ん) + だろうか (plain, with optional nominalizer)
    (b) => {
      const no = b.tok({ pos: 'SCONJ', dep: 'mark' }, 'no');
      const darou = b.aux({ lemma: 'だ', inflectionForm: '意志推量形', dep: 'fixed' }, 'darou');
      const ka = b.particle('か', 'ka');

      b.inOrder(no, darou, 1);
      b.inOrder(darou, ka, 1);

      b.captureSpan('のだろうか', no, ka);
    },

    // Pattern 2: な + (の/ん) + だろうか (for nouns/na-adjectives)
    (b) => {
      const na = b.aux({ lemma: 'だ', dep: 'cop' }, 'na');
      const no = b.tok({ pos: 'SCONJ', dep: 'mark' }, 'no');
      const darou = b.aux({ lemma: 'だ', inflectionForm: '意志推量形', dep: 'fixed' }, 'darou');
      const ka = b.particle('か', 'ka');

      b.inOrder(na, no, 1);
      b.inOrder(no, darou, 1);
      b.inOrder(darou, ka, 1);

      b.captureSpan('なのだろうか', na, ka);
    },

    // Pattern 3: だろうか (without nominalizer - old-fashioned but common)
    (b) => {
      const darou = b.aux({ lemma: 'だ', inflectionForm: '意志推量形' }, 'darou');
      const ka = b.particle('か', 'ka');

      b.inOrder(darou, ka, 1);

      b.captureSpan('だろうか', darou, ka);
    },

    // Pattern 4: (の/ん) + でしょうか (polite, with nominalizer)
    (b) => {
      const no = b.tok({ pos: 'SCONJ', dep: 'mark' }, 'no');
      const deshou = b.aux({ inflectionForm: '意志推量形', dep: 'fixed' }, 'deshou');
      const ka = b.particle('か', 'ka');

      b.inOrder(no, deshou, 1);
      b.inOrder(deshou, ka, 1);

      b.captureSpan('のでしょうか', no, ka);
    },

    // Pattern 5: な + (の/ん) + でしょうか (for nouns/na-adjectives, polite)
    (b) => {
      const na = b.aux({ lemma: 'だ', dep: 'cop' }, 'na');
      const no = b.tok({ pos: 'SCONJ', dep: 'mark' }, 'no');
      const deshou = b.aux({ inflectionForm: '意志推量形', dep: 'fixed' }, 'deshou');
      const ka = b.particle('か', 'ka');

      b.inOrder(na, no, 1);
      b.inOrder(no, deshou, 1);
      b.inOrder(deshou, ka, 1);

      b.captureSpan('なのでしょうか', na, ka);
    }
  );
});
