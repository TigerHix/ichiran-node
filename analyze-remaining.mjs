#!/usr/bin/env bun
import { GrammarEngine } from '/home/tiger/ichiran-node/packages/grammar/dist/program.js';
import { GinzaClient } from '/home/tiger/ichiran-node/packages/grammar/dist/ginza/client.js';

const client = new GinzaClient({ python: 'python3' });
await client.start();
const engine = await GrammarEngine.create([], { client });

const sentences = [
  '布団をあたらしくしたから気持ちいい。',
  '翻訳をおもしろくするアプリがある。',
  'お花見にはあたたかくして行った方がいい。',
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const doc = await engine.analyze(s);
  if (doc && doc.sentences.length > 0) {
    for (const tok of doc.sentences[0].tokens) {
      console.log(`  ${tok.text}: lemma=${tok.lemma}, pos=${tok.pos}, tag=${tok.tag}, inflectionForm=${tok.inflectionForm}, dep=${tok.dep}, head=${tok.head}`);
    }
  }
}

await client.stop();
