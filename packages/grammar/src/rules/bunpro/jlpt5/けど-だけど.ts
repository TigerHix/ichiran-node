import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('けど-だけど', (r) => {
  r.either(
    // Pattern 1: だけど (dakedo) - after noun or な-adjective (e.g., 便利だけど, 秋だけど)
    // Note: GiNZA tokenizes this as "だ" + "けど" as two separate tokens
    (r1) => {
      const head = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'ADJ', 'ADV', 'VERB'] }, 'head');
      const da = r1.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const kedo = r1.particle('けど', 'kedo', { depOneOf: ['dep', 'cc', 'mark'] });

      r1.inOrder(head, da, 2);
      r1.inOrder(da, kedo, 1);
      r1.headChild(head, kedo);

      r1.captureSpan('けど-だけど', head, kedo);
    },
    // Pattern 2: けど (kedo) - after verb, い-adjective, or clause (e.g., あるけど, 寒いけど, 泳ぐけど)
    (r2) => {
      const head = r2.tok({ posOneOf: ['VERB', 'ADJ'] }, 'head');
      const kedo = r2.particle('けど', 'kedo', { depOneOf: ['dep', 'cc', 'mark'] });

      r2.inOrder(head, kedo, 1);
      r2.headChild(head, kedo);

      r2.captureSpan('けど-だけど', head, kedo);
    },
    // Pattern 3: けれど (keredo) - slightly more formal variant of けど
    (r3) => {
      const head = r3.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN'] }, 'head');
      const keredo = r3.particle('けれど', 'keredo', { depOneOf: ['dep', 'cc', 'mark'] });

      r3.inOrder(head, keredo, 1);
      r3.headChild(head, keredo);

      r3.captureSpan('けど-だけど', head, keredo);
    },
    // Pattern 4: けれども (keredomo) - formal variant of けど
    (r4) => {
      const head = r4.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN'] }, 'head');
      const keredomo = r4.particle('けれども', 'keredomo', { depOneOf: ['dep', 'cc', 'mark'] });

      r4.inOrder(head, keredomo, 1);
      r4.headChild(head, keredomo);

      r4.captureSpan('けど-だけど', head, keredomo);
    }
  );
});
