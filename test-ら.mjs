import { analyze } from './packages/grammar/dist/program.js';

async function main() {
  const sentences = [
    '彼らは日本語を勉強しにきた。',
    '君ら池に入るつもり？',
    '僕らは全員日本に留学したことがあります。',
    'これらを捨ててください。',
    'それらの畳はいい匂いがしている。',
  ];

  for (const sentence of sentences) {
    console.log('\n========================================');
    console.log(`Sentence: ${sentence}`);
    console.log('========================================');
    const doc = await analyze(sentence);
    for (const tok of doc) {
      console.log(`  ${tok.text}: pos=${tok.pos}, lemma=${tok.lemma}, dep=${tok.dep}, head=${tok.head}`);
    }
  }
}

main();
