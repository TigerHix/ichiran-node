import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentence = 'あの白いドレスもありだし、ピンクのドレスも君に似合うよ。';

console.log(sentence);
console.log('='.repeat(80));
const doc = await engine.analyze(sentence);
if (doc && doc.sentences[0]) {
  const ariIdx = doc.sentences[0].tokens.findIndex(t => t.text === 'あり');
  console.log(`\nあり is at index ${ariIdx}, head=${doc.sentences[0].tokens[ariIdx].head}`);
  console.log(`Tokens around あり:`);
  for (let i = Math.max(0, ariIdx - 2); i <= Math.min(doc.sentences[0].tokens.length - 1, ariIdx + 3); i++) {
    const tok = doc.sentences[0].tokens[i];
    console.log(`  [${i}] ${tok.text.padEnd(8)} POS=${tok.pos.padEnd(6)} lemma=${tok.lemma.padEnd(10)} dep=${tok.dep.padEnd(8)} head=${tok.head}`);
  }

  // Check what token 4 is (what あり points to, or what points to あり)
  console.log(`\nToken structure:`);
  for (let i = 0; i < doc.sentences[0].tokens.length; i++) {
    const tok = doc.sentences[0].tokens[i];
    if (tok.head === ariIdx) {
      console.log(`  Token [${i}] "${tok.text}" (head=${tok.head}) points to [${ariIdx}] "あり"`);
    }
    if (i === ariIdx) {
      console.log(`  Token [${i}] "${tok.text}" (head=${tok.head}) points to [${tok.head}] "${doc.sentences[0].tokens[tok.head]?.text || 'ROOT'}"`);
    }
  }
}

await engine.close();
