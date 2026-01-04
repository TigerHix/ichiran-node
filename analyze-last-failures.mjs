#!/usr/bin/env bun
import { GrammarEngine } from './packages/grammar/dist/program.js';
import { GinzaClient } from './packages/grammar/dist/ginza/client.js';

const client = new GinzaClient({ python: 'python3' });
await client.start();
const engine = await GrammarEngine.create([], { client });

const sentences = [
  'この学校に入るのがずっと夢だったんだっけ？',
  '納豆を食べられないんだっけ？',
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const doc = await engine.analyze(s);
  if (doc && doc.sentences.length > 0) {
    for (const tok of doc.sentences[0].tokens) {
      console.log(`  ${tok.text}: lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, head=${tok.head}, tag=${tok.tag}, inf=${tok.inflectionForm}`);
    }
  }
}

await client.stop();
