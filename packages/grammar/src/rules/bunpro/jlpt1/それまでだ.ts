import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('それまでだ', (r) => {
  // Core pattern: それ + まで + だ
  const sore = r.tok({ text: 'それ' }, 'sore');
  const made = r.tok({ text: 'まで' }, 'made');
  const da = r.tok({ lemma: 'だ', pos: 'AUX' }, 'da');
  r.inOrder(sore, made, 1).inOrder(made, da, 1);

  // Variants for the prefix context
  r.either(
    (b) => {
      // Verb + たら/ば conditional
      // The verb must end in conditional form (たら or ば)
      const conditional = b.tok({
        posOneOf: ['AUX', 'SCONJ'],
        textOneOf: ['たら', 'ば'],
      }, 'conditional');
      b.inOrder(conditional, sore, 5); // Allow some distance for verb+aux chain
      b.captureSpan('それまでだ', conditional, da);
    },
    (b) => {
      // と言えば + それまでだ (fixed pattern)
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: '言う' }, 'iu');
      const ba = b.tok({ text: 'ば', pos: 'SCONJ' }, 'ba');
      b.inOrder(to, iu, 1);
      b.inOrder(iu, ba, 1);
      b.inOrder(ba, sore, 1);
      b.captureSpan('それまでだ', to, da);
    }
  );
});
