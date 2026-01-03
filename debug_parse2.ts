import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  'ない',
  '高くない',
  '食べない',
  '見ない',
];

for (const sentence of sentences) {
  console.log('\n=== ' + sentence + ' ===');
  const doc = await engine.analyze(sentence);
  for (const token of doc.sentences[0].tokens) {
    console.log(`  ${token.text}: lemma=${token.lemma}, pos=${token.pos}, conjClass=${token.conjugationClass}, inflForm=${token.inflectionForm}`);
  }
}

await engine.close();
