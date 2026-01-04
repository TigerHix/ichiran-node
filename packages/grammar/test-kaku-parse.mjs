import { GinzaClient } from './dist/ginza/client.js';

const client = new GinzaClient();

async function main() {
  const sentences = [
    'このバスはかくバス停に止まります。',
    'このバスは各バス停に止まります。',
    '各部屋に冷房がついている。',
  ];
  
  for (const sent of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log('Sentence:', sent);
    const doc = await client.analyze(sent);
    for (let i = 0; i < doc.tokens.length; i++) {
      const t = doc.tokens[i];
      const reading = t.reading ? ` reading="${t.reading}"` : '';
      console.log(`${i}: "${t.text}" lemma="${t.lemma}" pos="${t.pos}" dep="${t.dep}" head=${t.head}${reading}`);
    }
  }
}

main().catch(console.error);
