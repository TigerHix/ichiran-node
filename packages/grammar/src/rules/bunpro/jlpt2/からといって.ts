import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からといって (karatoitte) - Just because, although
 */
export default linguisticRule('からといって', (r) => {
  r.either(
    // Pattern 1: Noun + だ + から + と + いって (だからといって)
    (r1) => {
      const noun = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = r1.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const kara = r1.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'] }, 'kara');
      const to = r1.tok({ text: 'と', posOneOf: ['ADP', 'PART'] }, 'to');
      const itte = r1.verb({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      r1.inOrder(noun, da, 1);
      r1.inOrder(da, kara, 1);
      r1.inOrder(kara, to, 1);
      r1.inOrder(to, itte, 1);

      r1.captureSpan('からといって', da, itte);
    },

    // Pattern 2: な-Adjective + だ + から + と + いって
    (r2) => {
      const naAdj = r2.adj({}, 'naAdj');
      const da = r2.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const kara = r2.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'] }, 'kara');
      const to = r2.tok({ text: 'と', posOneOf: ['ADP', 'PART'] }, 'to');
      const itte = r2.verb({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      r2.inOrder(naAdj, da, 1);
      r2.inOrder(da, kara, 1);
      r2.inOrder(kara, to, 1);
      r2.inOrder(to, itte, 1);

      r2.captureSpan('からといって', da, itte);
    },

    // Pattern 3: い-Adjective + から + と + いって
    (r3) => {
      const iAdj = r3.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const kara = r3.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'] }, 'kara');
      const to = r3.tok({ text: 'と', posOneOf: ['ADP', 'PART'] }, 'to');
      const itte = r3.verb({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      r3.inOrder(iAdj, kara, 1);
      r3.inOrder(kara, to, 1);
      r3.inOrder(to, itte, 1);

      r3.captureSpan('からといって', iAdj, itte);
    },

    // Pattern 4: Verb + から + と + いって
    (r4) => {
      const verb = r4.verb({}, 'verb');
      const kara = r4.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'] }, 'kara');
      const to = r4.tok({ text: 'と', posOneOf: ['ADP', 'PART'] }, 'to');
      const itte = r4.verb({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      r4.inOrder(verb, kara, 5);
      r4.inOrder(kara, to, 1);
      r4.inOrder(to, itte, 1);

      r4.captureSpan('からといって', verb, itte);
    }
  );
});
