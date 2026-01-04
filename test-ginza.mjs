import { analyze } from './packages/grammar/src/ginza/client.js';

const sentence = 'たべようとおもう。';
console.log('Analyzing:', sentence);
const doc = await analyze(sentence);
doc.tokens.forEach((t, i) => {
  console.log(`[${i}] "${t.text}" | pos=${t.pos} | dep=${t.dep} | lemma=${t.lemma} | inflectionForm=${t.inflectionForm}`);
});
