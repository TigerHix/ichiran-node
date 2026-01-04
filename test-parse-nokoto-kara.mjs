import { GinzaClient } from './src/ginza/client.js';
const client = new GinzaClient({ python: 'python3' });
await client.start();

const sentences = [
  '以上のことから、この結論に至りました。',
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(`SENTENCE: ${sent}`);
  console.log('='.repeat(80));
  const docs = await client.analyze([sent]);
  // Show tokens around "のことから"
  const tokens = docs[0].sentences[0].tokens;
  const kotoIdx = tokens.findIndex(t => t.text === 'こと');
  if (kotoIdx >= 0) {
    console.log('\nContext around こと:');
    for (let i = Math.max(0, kotoIdx - 3); i <= Math.min(tokens.length - 1, kotoIdx + 2); i++) {
      const t = tokens[i];
      const inf = t.inflectionForm || 'N/A';
      console.log(`  [${i}] ${t.text} (pos=${t.pos}, lemma=${t.lemma}, dep=${t.dep}, head=${t.head}, inflectionForm=${inf})`);
    }
  }
}

await client.stop();
