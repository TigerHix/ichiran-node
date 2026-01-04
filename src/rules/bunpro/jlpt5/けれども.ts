import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('けれども', (r) => {
  // けれども is a formal conjunction meaning "but/however"
  // Variants: けれども, けれど, だけれども, だけれど, けども
  // Similar to が-but but more formal

  r.either(
    // Pattern 1: だけれども / だけれど (after noun/na-adjective + copula だ)
    (b) => {
      const da = b.tok({ lemma: 'だ' }, 'da');
      const keredomo = b.tok({
        textOneOf: ['けれども', 'けれど'],
        depOneOf: ['dep', 'cc', 'mark']
      }, 'keredomo');
      b.inOrder(da, keredomo, 1);
      b.captureSpan('だけれども', da, keredomo);
    },
    // Pattern 2: んだけれども / んだけれど (after explanatory ん)
    (b) => {
      const n = b.aux({ lemma: 'ん' }, 'n');
      const keredomo = b.tok({
        textOneOf: ['けれども', 'けれど'],
        depOneOf: ['dep', 'cc', 'mark']
      }, 'keredomo');
      b.inOrder(n, keredomo, 1);
      b.captureSpan('んだけれども', n, keredomo);
    },
    // Pattern 3: けれども / けれど (after verbs/i-adjectives)
    (b) => {
      const keredomo = b.tok({
        textOneOf: ['けれども', 'けれど'],
        depOneOf: ['dep', 'cc', 'mark']
      }, 'keredomo');
      b.capture(keredomo);
    },
    // Pattern 4: けども (very short variant)
    (b) => {
      const kedo = b.tok({ text: 'けど' }, 'kedo');
      const mo = b.particle('も', 'mo');
      b.inOrder(kedo, mo, 1);
      b.captureSpan('けども', kedo, mo);
    }
  );
});
