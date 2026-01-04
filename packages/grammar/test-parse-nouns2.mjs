import { GrammarEngine } from './src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });
  
  const sentences = [
    '彼はいつも遠慮がちだ。',
    '私の息子は病気がちだ。',
  ];
  
  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const doc = await engine.analyze(sent);
    if (!doc) {
      console.log('  ERROR: doc is null');
      continue;
    }
    for (const sentence of doc.sentences) {
      sentence.tokens.forEach((t, i) => {
        console.log(`  ${i}: ${t.text} | pos=${t.pos} | lemma=${t.lemma} | tag=${t.tag} | inf=${t.inflectionForm} | dep=${t.dep}`);
      });
    }
  }
  
  await engine.close();
}

main().catch(console.error);
