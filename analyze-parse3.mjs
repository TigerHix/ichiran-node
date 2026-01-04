import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const client = new GinzaClient();

const sent = '看板に載っている漢字の読み方をおしえてくれてありがとう';
console.log('=== ' + sent + ' ===');
const result = await client.analyze([sent]);
const tokens = result["0"].sentences[0].tokens;
for (const t of tokens) {
  console.log(`${t.i}: ${t.text}\t${t.pos}\t${t.lemma}\t${t.dep}\thead=${t.head}`);
}
