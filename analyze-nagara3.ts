import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const sentences = [
  '音楽を聴きながら走ります。',
  '運転しながら携帯を使っていたら。',
  '私が勉強しながら働く。',
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
        if (tok.text === 'ながら' || (tok.inflectionForm === '連用形-一般' && (tok.pos === 'VERB' || tok.pos === 'AUX'))) {
          console.log(`${tok.i}: ${tok.text} (lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, inflectionForm=${tok.inflectionForm}, head=${tok.head})`);
        }
      }
    }
  }

  await client.stop();
}

main().catch(console.error);
