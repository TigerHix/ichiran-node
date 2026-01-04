import { GrammarEngine } from './packages/grammar/src/program.js';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Find ginza worker
const ginzaWorker = join(__dirname, 'packages/grammar/src/ginza/worker.py');

const engine = GrammarEngine.create([], { 
  ginzaWorker 
});

const sentence = '妹が孫のように甘える。';
const doc = await engine.analyze(sentence);

console.log('Sentence:', sentence);
console.log('Tokens:');
for (const token of doc.tokens) {
  const tag = token.tag || 'N/A';
  const inf = token.inflectionForm || 'N/A';
  console.log(`  "${token.text}" [idx=${token.i}]: pos=${token.pos}, lemma=${token.lemma}, tag=${tag}, dep=${token.dep}, inflectionForm=${inf}`);
}
