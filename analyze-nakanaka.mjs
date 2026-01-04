#!/usr/bin/env node
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { join, dirname } from 'node:path';

// Load the grammar module's GiNZA client
const grammarDir = join(dirname(fileURLToPath(import.meta.url)), 'packages/grammar');
const { GiNZA } = await import('@google-cloud/gi-nz');

const ginza = new GiNZA();

const sentences = [
  'ここのラーメンはなかなか美味しいね。',
  'ミムラさんもなかなか可愛いよ。',
  'あのシェフが作るパスタはなかなかの物だ。',
  '元カノはなかなかの美人でしたが、性格が悪かったので別れました。',
  'この作品は中々の出来栄えですよ。',
  '富士登山は中々大変です。',
];

for (const sentence of sentences) {
  console.log(`\n${'='.repeat(60)}`);
  console.log(`  ${sentence}`);
  console.log('='.repeat(60));

  const doc = await ginza.run(sentence);

  for (const token of doc.tokens) {
    console.log(`  ${token.text.padEnd(10)} lemma=${token.lemma.padEnd(10)} pos=${token.pos.padEnd(6)} tag=${token.tag.padEnd(25)} dep=${token.dep} (${token.head})`);
  }
}

await ginza.close();
