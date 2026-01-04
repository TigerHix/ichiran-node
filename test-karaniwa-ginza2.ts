import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const sentences = [
  '日本に住むからは、日本語を勉強するべきだ。',
  '教師であるからは、生徒の手本となるべし。',
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
