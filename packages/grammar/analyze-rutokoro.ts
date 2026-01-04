import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

const engine = useSharedEngine([]);

const sentences = [
  '今から帰るところです',
  '出かけるところだ',
  '友達と会うところです',
  '食べるところで、焦げた匂いがした',
  '宿題をするところで、友達から電話が入った',
];

async function main() {
  console.log('=== ANALYZING るところだ PATTERN ===\n');

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`Sentence: ${sentence}`);
    console.log('='.repeat(80));

    const doc = await engine.analyze(sentence);
    console.log('Tokens:');
    doc.tokens.forEach((t, i) => {
      const depStr = t.dep ?? '-';
      const inflStr = t.inflectionForm ?? '-';
      console.log(`  [${i}] ${t.text.padEnd(12)} POS=${t.pos.padEnd(6)} lemma=${t.lemma.padEnd(10)} dep=${depStr.padEnd(8)} infl=${inflStr.padEnd(20)} head=${t.head}`);
    });
  }
}

main().catch(console.error);
