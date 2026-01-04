import { GrammarEngine } from './src/program.js';
import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const engine = new GrammarEngine([], { client });

const sentences = [
  '日本は住みやすい。それに病院代が安い。',
  'それに彼は家事もしないんでしょう？',
];

for (const sent of sentences) {
  console.log('\n=== ' + sent + ' ===');
  const doc = await engine.analyze(sent);
  
  for (const tok of doc.tokens) {
    console.log(`${tok.text} pos=${tok.pos} dep=${tok.dep} lemma=${tok.lemma}`);
  }
}

await client.close();
