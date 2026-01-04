#!/usr/bin/env bun
import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const sentences = [
  '犯人が捕まることなく１０年が経つ。',
  '遅刻することなく、職場に着いた。',
  '彼は社長に何も言うことなく会社を辞めた。',
];

for (const sentence of sentences) {
  console.log('\n=== ' + sentence + ' ===\n');
  const [result] = await client.analyze([sentence]);
  const tokens = result.sentences[0].tokens;

  // Find verb before こと
  const kotoToken = tokens.find(t => t.lemma === 'こと' && t.pos === 'NOUN');
  if (kotoToken) {
    console.log(`こと token at index ${kotoToken.i}:`);
    console.log(`  text=${kotoToken.text}, pos=${kotoToken.pos}, lemma=${kotoToken.lemma}, dep=${kotoToken.dep}, head=${kotoToken.head}`);

    // Check which tokens point to this こと
    const children = tokens.filter(t => t.head === kotoToken.i);
    console.log(`  Children (head=${kotoToken.i}):`);
    for (const child of children) {
      console.log(`    ${child.i}: ${child.text} (pos=${child.pos}, dep=${child.dep})`);
    }

    // Check the token before こと
    if (kotoToken.i > 0) {
      const prev = tokens[kotoToken.i - 1];
      console.log(`  Previous token: ${prev.i}: ${prev.text} (pos=${prev.pos}, lemma=${prev.lemma}, dep=${prev.dep}, head=${prev.head})`);
    }
  }

  // Find なく token
  const nakuToken = tokens.find(t => t.text === 'なく' && t.lemma === 'ない');
  if (nakuToken) {
    console.log(`\nなく token at index ${nakuToken.i}:`);
    console.log(`  text=${nakuToken.text}, pos=${nakuToken.pos}, lemma=${nakuToken.lemma}, dep=${nakuToken.dep}, head=${nakuToken.head}, inflection=${nakuToken.inflection}`);
  }
}

await client.stop();
