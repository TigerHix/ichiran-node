#!/usr/bin/env bun
import { GrammarEngine } from './src/program.js';

const client = new (await import('./src/ginza/client.js')).GinzaClient();
await client.start();
const engine = await GrammarEngine.create([], { client });

async function analyzeSentence(sentence) {
  console.log('\n========================================');
  console.log(`Sentence: ${sentence}`);
  console.log('========================================');

  const doc = await engine.analyze(sentence);

  if (!doc) {
    console.log('No doc returned');
    return;
  }

  console.log('\nTokens:');
  for (const sent of doc.sentences) {
    sent.tokens.forEach((token, i) => {
      const extras = [];
      if (token.inflectionForm) extras.push(`inf=${token.inflectionForm}`);
      if (token.conjugationClass) extras.push(`conj=${token.conjugationClass}`);
      console.log(`  [${i}] ${token.text.padEnd(10)} lemma=${token.lemma.padEnd(10)} pos=${token.pos.padEnd(6)} dep=${token.dep.padEnd(8)} head=${token.head} ${extras.join(' ')}`);
    });
  }
}

await analyzeSentence('行くかどうかわかりません。');
await analyzeSentence('来るか分かる？');

await client.stop();
