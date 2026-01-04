import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const sentences = [
  '温泉に行くからには、美味しい料理を出してくれる宿に泊まりたい。',
  '休業するからには、事前の説明をきちんとしてください。',
];

const result = await client.analyze(sentences);
for (const doc of result) {
  for (const sent of doc.sentences) {
    console.log('Sentence:', sent.text);
    for (const tok of sent.tokens) {
      const infl = tok.inflection ? tok.inflection : '(none)';
      console.log('  [' + tok.i + '] ' + tok.text + ' | lemma=' + tok.lemma + ' | pos=' + tok.pos + ' | dep=' + tok.dep + ' | inflection=' + infl + ' | head=' + tok.head);
    }
    console.log();
  }
}

await client.stop();
