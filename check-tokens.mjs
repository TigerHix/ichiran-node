// Check token structure using the helper
import { GinzaClient } from './packages/grammar/src/ginza/client.ts';

const client = new GinzaClient();

await client.start();

const sent = 'そんな歩き方をしていては、ペンギンかと思われますよ。';
console.log('===', sent, '===\n');

const [doc] = await client.analyze([sent]);
console.log('Sentences:', doc.sentences.length);
const sentence = doc.sentences[0];
console.log('Tokens:', sentence.tokens.length);
sentence.tokens.forEach((t, i) => {
  console.log(`${i}: "${t.text}" lemma="${t.lemma}" pos=${t.pos} head=${t.head} dep=${t.dep}`);
});

await client.stop();
