#!/usr/bin/env bun
import { GinzaClient } from './packages/grammar/src/ginza/client.ts';

const client = new GinzaClient();

const sentences = [
  '寝る前にそんなコーヒーをのんでは寝られない。',
];

const docs = await client.analyze(sentences);

for (let di = 0; di < docs.length; di++) {
  const doc = docs[di];
  const sent = sentences[di];
  console.log('\n========================================');
  console.log(`Sentence: ${sent}`);
  console.log('========================================');

  for (const sent of doc.sentences) {
    sent.tokens.forEach((t, i) => {
      console.log(`${i}: text="${t.text}" lemma="${t.lemma}" pos=${t.pos} dep=${t.dep} head=${t.head} inflection="${t.inflectionForm || 'none'}"`);
    });
  }
}

await client.stop();
