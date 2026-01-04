import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const sentences = [
  '休業するからには、事前の説明をきちんとしてください。',
  '時間を延長するからには、それなりの料金を支払う必要があります。',
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
