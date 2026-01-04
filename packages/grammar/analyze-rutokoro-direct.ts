import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();

const sentences = [
  '今から帰るところです',
  '出かけるところだ',
  '友達と会うところです',
  '食べるところで、焦げた匂いがした',
  '宿題をするところで、友達から電話が入った',
  'ちょうど出かけるところだ',
];

async function main() {
  console.log('=== ANALYZING るところだ PATTERN ===\n');

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`Sentence: ${sentence}`);
    console.log('='.repeat(80));

    const [doc] = await client.analyze([sentence]);

    for (const sent of doc.sentences) {
      console.log('Tokens:');
      sent.tokens.forEach((t, i) => {
        const depStr = t.dep ?? '-';
        const inflStr = t.inflectionForm ?? '-';
        console.log(`  [${i}] ${t.text.padEnd(12)} POS=${t.pos.padEnd(6)} lemma=${t.lemma.padEnd(10)} dep=${depStr.padEnd(8)} infl=${inflStr.padEnd(20)} head=${t.head}`);
      });
    }
  }

  await client.close();
}

main().catch(console.error);
