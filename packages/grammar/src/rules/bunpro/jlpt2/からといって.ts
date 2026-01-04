import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からといって (karatoitte) - Just because, although
 */
export default linguisticRule('からといって', (r) => {
  r.either(
    // Pattern 1: Noun/な-Adj + だ + から + と + いって (full form as separate tokens)
    (r1) => {
      const nounOrAdj = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'] }, 'nounOrAdj');
      const da = r1.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const kara = r1.tok({ text: 'から' }, 'kara');
      const to = r1.tok({ text: 'と' }, 'to');
      const itte = r1.tok({ text: 'いって' }, 'itte');

      r1.inOrder(nounOrAdj, da, 1);
      r1.inOrder(da, kara, 1);
      r1.inOrder(kara, to, 1);
      r1.inOrder(to, itte, 1);

      r1.captureSpan('からといって', da, itte);
    },

    // Pattern 2: い-Adj + から + と + いって (full form)
    (r2) => {
      const iAdj = r2.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const kara = r2.tok({ text: 'から' }, 'kara');
      const to = r2.tok({ text: 'と' }, 'to');
      const itte = r2.tok({ text: 'いって' }, 'itte');

      r2.inOrder(iAdj, kara, 1);
      r2.inOrder(kara, to, 1);
      r2.inOrder(to, itte, 1);

      r2.captureSpan('からといって', iAdj, itte);
    },

    // Pattern 3: Verb + から + と + いって (full form)
    (r3) => {
      const verb = r3.verb({}, 'verb');
      const kara = r3.tok({ text: 'から' }, 'kara');
      const to = r3.tok({ text: 'と' }, 'to');
      const itte = r3.tok({ text: 'いって' }, 'itte');

      r3.inOrder(verb, kara, 5);
      r3.inOrder(kara, to, 1);
      r3.inOrder(to, itte, 1);

      r3.captureSpan('からといって', verb, itte);
    },

    // Pattern 4: Noun/な-Adj + だ + からって (abbreviated as single token)
    (r4) => {
      const nounOrAdj = r4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'] }, 'nounOrAdj');
      const da = r4.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const karatte = r4.tok({ text: 'からって' }, 'karatte');

      r4.inOrder(nounOrAdj, da, 1);
      r4.inOrder(da, karatte, 1);

      r4.captureSpan('からといって', da, karatte);
    },

    // Pattern 5: い-Adj + からって (abbreviated)
    (r5) => {
      const iAdj = r5.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const karatte = r5.tok({ text: 'からって' }, 'karatte');

      r5.inOrder(iAdj, karatte, 1);

      r5.captureSpan('からといって', iAdj, karatte);
    },

    // Pattern 6: Verb + からとて (literary abbreviated)
    (r6) => {
      const verb = r6.verb({}, 'verb');
      const karatote = r6.tok({ text: 'からとて' }, 'karatote');

      r6.inOrder(verb, karatote, 5);

      r6.captureSpan('からといって', verb, karatote);
    }
  );
});
