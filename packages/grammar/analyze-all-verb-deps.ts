import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();

const sentences = [
  '出かけるところだ',
  '帰るところです',
  '会うところです',
  '食べるところで、焦げた匂いがした',
  'するところで、友達から電話が入った',
  '習い始めるところです',
];

async function main() {
  for (const sentence of sentences) {
    const [doc] = await client.analyze([sentence]);

    console.log(`\nSentence: ${sentence}`);

    for (const sent of doc.sentences) {
      console.log('  Verbs in 連体形-一般 before ところ:');
      const tokoroIdx = sent.tokens.findIndex(t => t.lemma === 'ところ');
      sent.tokens.filter(t => t.inflectionForm === '連体形-一般' && t.i < tokoroIdx).forEach(t => {
        console.log(`    ${t.text} (lemma=${t.lemma}, dep=${t.dep}, head=${t.head})`);
      });
    }
  }
}

main().catch(console.error);
