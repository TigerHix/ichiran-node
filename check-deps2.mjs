#!/usr/bin/env bun
import { GrammarEngine } from './packages/grammar/dist/program.js';
import { GinzaClient } from './packages/grammar/dist/ginza/client.js';

const client = new GinzaClient({ python: 'python3' });
await client.start();
const engine = await GrammarEngine.create([], { client});

const s = '納豆を食べられないんだっけ？';
console.log('=== ' + s + ' ===');
const doc = await engine.analyze(s);
if (doc && doc.sentences.length > 0) {
  const tokens = doc.sentences[0].tokens;
  tokens.forEach((tok, i) => {
    console.log(`${i}: ${tok.text} -> head=${tok.head} (text=${tokens[tok.head]?.text || 'ROOT'}) pos=${tok.pos}`);
  });
}

await client.stop();
