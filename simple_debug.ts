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

      if (doc) {
        console.log('All tokens:');
        for (let i = 0; i < doc.tokens.length; i++) {
          const token = doc.tokens[i];
          console.log(`  [${i}] "${token.text}": pos=${token.pos.padEnd(8)} dep=${token.dep.padEnd(8)} lemma=${token.lemma.padEnd(10)} head=[${token.head}] "${doc.tokens[token.head]?.text}"`);
        }
      }
    }
  } finally {
    await engine.close();
  }
}

main().catch(console.error);
