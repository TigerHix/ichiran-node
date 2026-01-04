import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();

async function test() {
  await client.start();
  
  const sentences = [
    'あの車はキムの車なのでしょうか。',
    'そんな高い鉛筆をトムが買うのでしょうか。',
  ];

  for (const sent of sentences) {
    console.log(`\n=== ${sent} ===`);
    const docs = await client.analyze([sent]);
    const tokens = docs[0].sentences[0].tokens;
    for (const tok of tokens) {
      const inf = tok.inflectionForm || '(none)';
      console.log(`${tok.text}\t${tok.pos}\t${tok.lemma}\t${tok.dep}\t${inf}`);
    }
  }
  
  await client.stop();
}

test().then(() => process.exit(0));
