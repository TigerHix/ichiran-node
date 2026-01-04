import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();

const sentences = [
  '今勉強しているところです。',  // should NOT match るところだ (is ているところだ)
  '出かけるところだ',  // should match るところだ
];

async function main() {
  console.log('=== ANALYZING ているところ vs るところだ ===\n');

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

      console.log('\nDependency relationships for verbs in 連体形-一般:');
      sent.tokens.filter(t => t.inflectionForm === '連体形-一般').forEach((t, i) => {
        console.log(`  Verb: ${t.text} (index ${t.i}, head=${t.head})`);
        // Find children pointing to this token
        sent.tokens.filter(c => c.head === t.i).forEach(child => {
          console.log(`    <- ${child.text} (dep=${child.dep}, pos=${child.pos})`);
        });
      });
    }
  }
}

main().catch(console.error);
