import { GiNZA } from './packages/grammar/src/ginza/client.js';

async function main() {
  const ginza = new GiNZA();

  const sentences = [
    '毎日走るが、運動はきらいです。',
    '私が行きます。',
  ];

  for (const sentence of sentences) {
    console.log('\n=== ' + sentence + ' ===');
    const doc = await ginza.parse(sentence);

    console.log('All tokens:');
    for (let i = 0; i < doc.tokens.length; i++) {
      const token = doc.tokens[i];
      console.log(`  [${i}] ${token.text}: pos=${token.pos}, dep=${token.dep}, lemma=${token.lemma}, head=${token.head}("${doc.tokens[token.head]?.text}")`);
    }
  }
}

main().catch(console.error);
