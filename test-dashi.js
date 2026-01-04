import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentence = 'あの白いドレスもありだし、ピンクのドレスも君に似合うよ。';

console.log(sentence);
console.log('='.repeat(80));
const doc = await engine.analyze(sentence);
if (doc && doc.sentences[0]) {
  for (const tok of doc.sentences[0].tokens) {
    console.log(`${tok.text.padEnd(10)} POS=${tok.pos.padEnd(6)} lemma=${tok.lemma.padEnd(10)} dep=${tok.dep.padEnd(8)} head=${tok.head}`);
  }
}

await engine.close();
