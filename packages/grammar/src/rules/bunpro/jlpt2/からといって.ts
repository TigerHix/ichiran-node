import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からといって (karatoitte) - Just because, although
 */
export default linguisticRule('からといって', (r) => {
  r.either(
    // Pattern 1: Noun/な-Adj + だ + からといって (single token)
    (r1) => {
      const nounOrAdj = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'] }, 'nounOrAdj');
      const da = r1.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const karatoitte = r1.tok({ textOneOf: ['からといって', 'からって', 'からとて'] }, 'karatoitte');

      r1.inOrder(nounOrAdj, da, 1);
      r1.inOrder(da, karatoitte, 1);

      r1.captureSpan('からといって', da, karatoitte);
    },

    // Pattern 2: い-Adj + からといって (single token)
    (r2) => {
      const iAdj = r2.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const karatoitte = r2.tok({ textOneOf: ['からといって', 'からって', 'からとて'] }, 'karatoitte');

      r2.inOrder(iAdj, karatoitte, 1);

      r2.captureSpan('からといって', iAdj, karatoitte);
    },

    // Pattern 3: Verb + からといって (single token)
    (r3) => {
      const verb = r3.verb({}, 'verb');
      const karatoitte = r3.tok({ textOneOf: ['からといって', 'からって', 'からとて'] }, 'karatoitte');

      r3.inOrder(verb, karatoitte, 5);

      r3.captureSpan('からといって', verb, karatoitte);
    }
  );
});
