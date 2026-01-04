import { GrammarEngine } from './packages/grammar/src/index.js';

async function debug() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });

  const sentence = '暑がりなくせに、あの人は毎日セーターを着て仕事に行く。';
  console.log(`Sentence: ${sentence}`);
  const doc = await engine.analyze(sentence);

  console.log('\nAll tokens:');
  for (const token of doc.sentences[0].tokens) {
    console.log(`  ${token.i}: "${token.text}" (pos=${token.pos}, lemma=${token.lemma}, dep=${token.dep})`);
  }

  await engine.close();
}

debug().catch(console.error);
