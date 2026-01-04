import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();

async function main() {
  const sentence = '新しい言語を習い始めるところです。';
  const [doc] = await client.analyze([sentence]);

  console.log('Sentence:', sentence);
  console.log('\nTokens:');
  for (const sent of doc.sentences) {
    sent.tokens.forEach((t, i) => {
      const depStr = t.dep ?? '-';
      const inflStr = t.inflectionForm ?? '-';
      console.log(`  [${i}] ${t.text.padEnd(12)} POS=${t.pos.padEnd(6)} lemma=${t.lemma.padEnd(12)} dep=${depStr.padEnd(8)} infl=${inflStr.padEnd(25)} head=${t.head}`);
    });

    console.log('\nVerbs in 連体形-一般:');
    sent.tokens.filter(t => t.inflectionForm === '連体形-一般').forEach(t => {
      console.log(`  ${t.text} (lemma=${t.lemma}, dep=${t.dep}, head=${t.head})`);
    });
  }
}

main().catch(console.error);
