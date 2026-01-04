import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だけでなく', (r) => {
  const dake = r.tok({ lemma: 'だけ' }, 'dake');
  // GiNZA often analyzes だけでなく as a fixed expression; "で" comes out as AUX (lemma=だ, dep=fixed).
  const de = r.tok({ text: 'で' }, 'de');
  const nai = r.aux({ lemma: 'ない' }, 'nai');
  // Often appears as an idiomatic contiguous chunk.
  r.inOrder(dake, de, 1).inOrder(de, nai, 2);
  r.captureSpan('だけでなく', dake, nai);
});

