import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  '私はカラオケで歌を歌わない。',
  '彼女は川で泳がない。',
  '今日はお酒を飲まない。',
];

for (const sentence of sentences) {
  console.log('\n=== ' + sentence + ' ===');
  const doc = await engine.analyze(sentence);
  for (const token of doc.sentences[0].tokens) {
    console.log(`  ${token.text}: lemma=${token.lemma}, pos=${token.pos}, conjClass=${token.conjugationClass}, inflForm=${token.inflectionForm}, dep=${token.dep}, head=${token.head}`);
  }
}

await engine.close();
