import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient({ python: 'python3' });
await client.start();

const sentences = [
  '勉強したら、テストは簡単になる。',
  '週末だったら、時間ありますよ。',
  'からかったら、牛乳を飲んでください。',
  '寒かったら、エアコンつけてね。',
  '彼の事が好きだったら、彼に言った方がいい。',
];

for (const sentence of sentences) {
  console.log(`\n=== ${sentence} ===`);
  const docs = await client.analyze([sentence]);
  const doc = docs[0];
  for (const s of doc.sentences) {
    for (const token of s.tokens) {
      console.log(`  [${token.i}] ${token.text} (lemma=${token.lemma}, pos=${token.pos}, tag=${token.tag}, dep=${token.dep}, head=${token.head}, inflForm=${token.inflectionForm})`);
    }
  }
}

await client.stop();
