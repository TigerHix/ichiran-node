import { GinzaClient } from './packages/grammar/src/ginza/client.ts';

const client = new GinzaClient();
await client.start();

const sent = '毎晩遊んでいてはお金はたまりませんよ。';
console.log('===', sent, '===');
const [doc] = await client.analyze([sent]);
const sentence = doc.sentences[0];

sentence.tokens.forEach((t, i) => {
  if (i < 10) {
    const headToken = sentence.tokens[t.head];
    console.log(`${i}: "${t.text}" lemma="${t.lemma}" pos=${t.pos} head=${t.head}(${headToken?.text}) dep=${t.dep}`);
  }
});

await client.stop();
