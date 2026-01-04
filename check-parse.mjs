import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const sentence = '車できてほしかった。';
const docs = await client.analyze([sentence]);
const doc = docs[0];
const parsed = doc.sentences[0];

console.log(`Sentence: ${sentence}`);
console.log('\nGiNZA Parse:');
for (let i = 0; i < parsed.tokens.length; i++) {
  const t = parsed.tokens[i];
  const text = t.text.padEnd(12);
  const lemma = t.lemma.padEnd(12);
  const pos = t.pos.padEnd(8);
  const dep = t.dep.padEnd(10);
  const inf = t.inflectionForm || '-';
  console.log(`  [${i}] text=${text} lemma=${lemma} pos=${pos} dep=${dep} inflection=${inf} head=${t.head}`);
}

await client.close();
