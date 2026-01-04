// Simple test to analyze GiNZA parses for ことから sentences
import { GinzaClient } from './src/ginza/client.js';
const client = new GinzaClient({ python: 'python3' });
await client.start();

const sentences = [
  'コーヒーが冷たいことから、コーヒーが淹れられたのは結構前だと分かる。',
  '家の家具が全部新しいことから、彼はここに引っ越してきたばかりだと分かった。',
  '野球がとても上手なことから、将来は有名になりそうだ。',
  '以上のことから、この結論に至りました。',
  '犯行現場で凶器が見つかったことから、被告は２５年の懲役を宣告された。',
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(`SENTENCE: ${sent}`);
  console.log('='.repeat(80));
  const docs = await client.analyze([sent]);
  console.log(JSON.stringify(docs[0], null, 2));
}

await client.stop();
