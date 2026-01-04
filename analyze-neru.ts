import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const sentences = [
  '赤ちゃんが寝る。',
  '赤ちゃんが寝ている。',
  '赤ちゃんが寝ながら、おならをした。',
  '私が寝ます。',
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
        if (tok.lemma === '寝る' || tok.text === '寝' || tok.text === 'ね') {
          console.log(`${tok.i}: ${tok.text} (lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, inflectionForm=${tok.inflectionForm})`);
        }
      }
    }
  }

  await client.stop();
}

main().catch(console.error);
