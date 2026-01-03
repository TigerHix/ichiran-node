import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentence = '起こらない';

console.log('=== ' + sentence + ' ===');
const doc = await engine.analyze(sentence);
for (const token of doc.sentences[0].tokens) {
  console.log(`  ${token.text}: lemma=${token.lemma}, pos=${token.pos}, conjClass=${token.conjugationClass}, inflForm=${token.inflectionForm}, dep=${token.dep}, head=${token.head}`);
}

await engine.close();
