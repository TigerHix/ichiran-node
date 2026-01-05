import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('かい', (r) => {
  // かい - Masculine sentence-final question particle
  // Casual, blunt way to ask questions, typically used by men
  // GiNZA parses "かい" as two tokens: か + い
  // "か" has dep='mark' (sentence-final particle)
  // "い" has dep='mark', dep='root', or dep='advcl' depending on sentence structure

  const ka = r.particle('か', 'ka', { dep: 'mark' });
  const i = r.tok({ text: 'い', posOneOf: ['PART', 'ADJ'], depOneOf: ['mark', 'root', 'advcl'] }, 'i');

  r.inOrder(ka, i, 1);
  r.captureSpan('かい', ka, i);
});
