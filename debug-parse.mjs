import { analyze } from './packages/grammar/src/ginza/client.js';

const sentences = [
  '冷蔵庫に入っているチーズを誰も食べないから、たべようとおもう。',
  'このゲームはもうあきらめようとおもう。',
  '私は明日から電車で通おうとおもう。',
  '来月、バイクを買おうとおもう。',
];

for (const s of sentences) {
  console.log('\n===', s, '===');
  const doc = await analyze(s);
  doc.tokens.forEach((t, i) => {
    console.log(`[${i}] ${t.text} | pos=${t.pos} | dep=${t.dep} | lemma=${t.lemma} | inflectionForm=${t.inflectionForm}`);
  });
}
