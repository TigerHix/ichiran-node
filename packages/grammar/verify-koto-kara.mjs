import { GrammarEngine } from './src/program.js';
import { GinzaClient } from './src/ginza/client.js';
import kotoKaraRule from './src/rules/bunpro/jlpt3/ことから.js';

const client = new GinzaClient({ python: 'python3' });
await client.start();

const engine = GrammarEngine.create([{ id: 'test', rules: [kotoKaraRule] }], { client });

const testCases = [
  'コーヒーが冷たいことから、コーヒーが淹れられたのは結構前だと分かる。',
  '家の家具が全部新しいことから、彼はここに引っ越してきたばかりだと分かった。',
  '野球がとても上手なことから、将来は有名になりそうだ。',
  '以上のことから、この結論に至りました。',
];

for (const sent of testCases) {
  console.log('\n' + '='.repeat(80));
  console.log(`SENTENCE: ${sent}`);
  const hits = await engine.match(sent);
  if (hits.length > 0) {
    for (const hit of hits) {
      console.log(`✓ Matched rule: ${hit.ruleId}`);
      console.log(`  Captures:`);
      for (const [name, cap] of Object.entries(hit.captures)) {
        console.log(`    ${name}: "${cap.text}" (${cap.start}-${cap.end})`);
      }
    }
  } else {
    console.log(`✗ No match`);
  }
}

await client.stop();
