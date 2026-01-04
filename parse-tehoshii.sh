#!/bin/bash
cd /tmp/jlpt4-4

# Create a simpler script to just parse sentences
bun << 'BUNSCRIPT'
import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const sentences = [
  '一緒にたべにいってほしいんです。',
  'もう一回説明してほしいです。',
  '車できてほしかった。',
  '一緒に勉強してほしいです。',
  'コーラを買ってほしいです。',
  '来てほしい。',
  '私に見てほしい。',
  '待っていてほしい。',
  '手伝ってほしい。'
];

for (const sentence of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(`Sentence: ${sentence}`);
  console.log('='.repeat(80));

  const docs = await client.analyze([sentence]);
  const doc = docs[0];
  const parsed = doc.sentences[0];
  console.log('\nGiNZA Parse:');
  for (let i = 0; i < parsed.tokens.length; i++) {
    const t = parsed.tokens[i];
    const text = t.text.padEnd(12);
    const lemma = t.lemma.padEnd(12);
    const pos = t.pos.padEnd(8);
    const dep = t.dep.padEnd(10);
    const inf = t.inflectionForm || '-';
    const head = t.head;
    console.log(`  [${i}] text=${text} lemma=${lemma} pos=${pos} dep=${dep} inflection=${inf} head=${head}`);
  }
}

await client.close();
BUNSCRIPT
