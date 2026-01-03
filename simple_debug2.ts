import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' }
  });

  try {
    const sentence = '毎日走るが、運動はきらいです。';
    console.log('\n=== ' + sentence + ' ===');

    const result = await engine['client'].analyze([sentence]);
    console.log('Result:', result);
    console.log('Length:', result?.length);

    const doc = await engine.analyze(sentence);
    console.log('Doc:', doc);

  } finally {
    await engine.close();
  }
}

main().catch(console.error);
