import { GinzaClient } from './src/ginza/client.js';
const client = new GinzaClient({ python: 'python3' });
await client.start();

const sentences = [
  'ことから始める。',
  'このことから始まった。',
  'このことから考え直す。',
  '彼のことから考える。',
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(`SENTENCE: ${sent}`);
  console.log('='.repeat(80));
  const docs = await client.analyze([sent]);
  const tokens = docs[0].sentences[0].tokens;
  const kotoIdx = tokens.findIndex(t => t.text === 'こと');
  if (kotoIdx >= 0) {
    const t = tokens[kotoIdx];
    const inf = t.inflectionForm || 'N/A';
    console.log(`こと: pos=${t.pos}, lemma=${t.lemma}, dep=${t.dep}, head=${t.head}, inflectionForm=${inf}`);
    console.log(`Full tokens:`);
    tokens.forEach((tok, i) => {
      console.log(`  [${i}] ${tok.text} (pos=${tok.pos}, dep=${tok.dep}, head=${tok.head})`);
    });
  }
}

await client.stop();
