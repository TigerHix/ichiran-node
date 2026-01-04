import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const sentences = [
  '赤ちゃんがねながら、おならをした。',
  '運転しながら携帯を使っていたら。',
  '生まれながらの音楽家だ。',
  '音楽を聴きながら走ります。',
];

async function main() {
  const client = new GinzaClient();
  await client.start();

  for (const sentence of sentences) {
    console.log(`\n=== ${sentence} ===`);
    const docs = await client.analyze([sentence]);
    const doc = docs[0];
    for (const sent of doc.sentences) {
      // Find ながら
      const nagaraIdx = sent.tokens.findIndex(t => t.lemma === 'ながら');
      if (nagaraIdx >= 0) {
        const nagara = sent.tokens[nagaraIdx];
        const prev = sent.tokens[nagaraIdx - 1];
        const next = sent.tokens[nagaraIdx + 1];
        console.log(`  ながら: idx=${nagaraIdx}, dep=${nagara.dep}, head=${nagara.head}`);
        console.log(`  prev token: ${prev.text} (pos=${prev.pos}, dep=${prev.dep}, inflectionForm=${prev.inflectionForm})`);
        if (next) {
          console.log(`  next token: ${next.text} (pos=${next.pos}, dep=${next.dep})`);
        }
      }
    }
  }

  await client.stop();
}

main().catch(console.error);
