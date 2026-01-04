import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();
await client.start();

// Test sentences from the JSON
const testSentences = [
  '彼はわかりやすそうな説明をしたが、まだ理解ができない。',
];

for (const sent of testSentences) {
  console.log('\n=== ' + sent + ' ===');
  const docs = await client.analyze([sent]);
  if (!docs || !docs[0]) continue;
  const doc = docs[0];
  // Pretty print just the tokens we need from first sentence
  for (const s of doc.sentences) {
    for (const tok of s.tokens) {
      console.log(`  ${tok.text}: pos=${tok.pos}, lemma=${tok.lemma}, dep=${tok.dep}, head=${tok.head}, inflectionForm=${tok.inflectionForm}`);
    }
  }
}

await client.stop();
