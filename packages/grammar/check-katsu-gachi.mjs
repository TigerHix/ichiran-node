import { GrammarEngine } from './src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });
  
  const sent = '勝つがちの試合だった。';
  console.log('=== ' + sent + ' ===');
  const doc = await engine.analyze(sent);
  if (!doc) {
    console.log('  ERROR: doc is null');
  } else {
    for (const sentence of doc.sentences) {
      sentence.tokens.forEach((t, i) => {
        console.log(`  ${i}: ${t.text} | pos=${t.pos} | lemma=${t.lemma} | tag=${t.tag} | inf=${t.inflectionForm} | dep=${t.dep}`);
      });
    }
  }
  
  await engine.close();
}

main().catch(console.error);
