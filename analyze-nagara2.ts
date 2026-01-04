import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const sentences = [
  '運転しながら携帯を使っていたら。',
  '赤ちゃんがねながら、おならをした。',
  '生まれながらの音楽家だ。',
];

async function main() {
  const client = new GinzaClient();
  await client.start();

  for (const sentence of sentences) {
    console.log(`\n=== ${sentence} ===`);
    const docs = await client.analyze([sentence]);
    const doc = docs[0];
    for (const sent of doc.sentences) {
      for (const tok of sent.tokens) {
        console.log(`${tok.i}: ${tok.text} (lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, inflectionForm=${tok.inflectionForm})`);
      }
    }
  }

  await client.stop();
}

main().catch(console.error);
