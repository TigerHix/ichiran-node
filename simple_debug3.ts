import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' }
  });

  try {
    const sentences = [
      '毎日走るが、運動はきらいです。',
      '私が行きます。',
      'お金は大切だが、時間も大切だ。',
    ];

    for (const sentence of sentences) {
      console.log('\n=== ' + sentence + ' ===');
      const doc = await engine.analyze(sentence);

      if (doc && doc.sentences && doc.sentences[0]) {
        console.log('All tokens:');
        const tokens = doc.sentences[0].tokens;
        for (let i = 0; i < tokens.length; i++) {
          const token = tokens[i];
          console.log(`  [${i}] "${token.text}": pos=${token.pos.padEnd(8)} dep=${token.dep.padEnd(8)} lemma=${token.lemma.padEnd(10)} head=[${token.head}] "${tokens[token.head]?.text}"`);
        }
      }
    }
  } finally {
    await engine.close();
  }
}

main().catch(console.error);
