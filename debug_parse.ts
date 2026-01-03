import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  '歩かない',
  '歌わない',
  '泳がない',
  '話さない',
  '読まない',
  '知らない',
];

for (const sentence of sentences) {
  console.log('\n=== ' + sentence + ' ===');
  const doc = await engine.analyze(sentence);
  console.log(JSON.stringify(doc, null, 2));
}

await engine.close();
