import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const sentence = '植木職人になるからには、辛い下積み生活も覚悟しなければいけない。';

const result = await client.analyze([sentence]);
for (const doc of result) {
  for (const sent of doc.sentences) {
    console.log('Sentence:', sent.text);
    for (const tok of sent.tokens) {
      const infl = tok.inflection ? tok.inflection : '(none)';
      console.log('  [' + tok.i + '] ' + tok.text + ' | lemma=' + tok.lemma + ' | pos=' + tok.pos + ' | dep=' + tok.dep + ' | inflection=' + infl + ' | head=' + tok.head);
    }
  }
}

await client.stop();
