import { GinzaClient } from './packages/grammar/src/ginza/client.ts';

const client = new GinzaClient();
await client.start();

const sentences = [
  '毎日お菓子ばかりを食べていては、いつまで経っても痩せませんよ。',
  '毎晩遊んでいてはお金はたまりませんよ。',
  '太っていてはメンバーに入れない。',
];

for (const sent of sentences) {
  console.log('\n===', sent, '===');
  const [doc] = await client.analyze([sent]);
  const sentence = doc.sentences[0];
  
  // Find the いては pattern
  sentence.tokens.forEach((t, i) => {
    if (t.text === 'て' || t.text === 'い' || t.text === 'は' || t.text === 'で') {
      const headToken = sentence.tokens[t.head];
      console.log(`${i}: "${t.text}" lemma="${t.lemma}" pos=${t.pos} head=${t.head}(${headToken?.text}) dep=${t.dep}`);
    }
  });
}

await client.stop();
