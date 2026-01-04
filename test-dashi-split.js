import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  'あの白いドレスもありだし',
  'お酒もありだけど',
  'カツ丼や牛丼もありだな',
];

for (const s of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(s);
  console.log('='.repeat(80));
  const doc = await engine.analyze(s);
  if (doc && doc.sentences[0]) {
    for (let i = 0; i < doc.sentences[0].tokens.length; i++) {
      const tok = doc.sentences[0].tokens[i];
      console.log(`[${i}] ${tok.text.padEnd(8)} POS=${tok.pos.padEnd(6)} lemma=${tok.lemma.padEnd(10)} dep=${tok.dep.padEnd(8)} head=${tok.head}`);
    }
  }
}

await engine.close();
