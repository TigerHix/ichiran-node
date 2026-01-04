#!/usr/bin/env -S bun run

import { GrammarEngine } from './src/program.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/jlpt3/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT3]);
const sentence = process.argv[2] || '一本早い電車に乗ることにした。';
const doc = await engine.analyze(sentence);

console.log(`Sentence: ${sentence}`);
if (!doc || !doc.sentences[0]) {
  console.log('No doc or sentence returned');
  process.exit(1);
}
const sent = doc.sentences[0];
console.log('\nAll tokens:');
sent.tokens.forEach((t, i) => {
  console.log(`  [${i}] ${t.text.padEnd(10)} (pos=${t.pos.padEnd(6)} lemma=${t.lemma.padEnd(10)} dep=${t.dep.padEnd(6)} head=${t.head})`);
});
