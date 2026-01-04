import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const tests = [
  '乾杯と言わないで、のみだした。',
  '飲み出した。',
  '彼女は泣き出した。',
];

for (const text of tests) {
  console.log('\n========================================');
  const result = await client.analyze([text]);
  for (const doc of result) {
    for (const sent of doc.sentences) {
      console.log('Sentence:', sent.text);
      for (const tok of sent.tokens) {
        const infl = tok.inflection ? tok.inflection : '(none)';
        console.log('  [' + tok.i + '] ' + tok.text + ' | lemma=' + tok.lemma + ' | pos=' + tok.pos + ' | dep=' + tok.dep + ' | inflection=' + infl + ' | head=' + tok.head);
      }
    }
  }
}

await client.stop();
