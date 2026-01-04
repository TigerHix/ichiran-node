import { GinzaClient } from './packages/grammar/src/ginza/client.ts';

const client = await GinzaClient.create();

const testSentences = [
  'コンピュータは以前に比べてずっと使いやすくなった。',
  'ずっと好きです。',
  'ずっと一緒にいる。',
  '点数が伸びれば、あのチームは合格ラインに到達するそうだ。',
];

for (const sent of testSentences) {
  console.log('\n=== ' + sent + ' ===');
  const doc = await client.parse(sent);
  for (const token of doc.tokens) {
    console.log(`${token.text}\t${token.pos}\t${token.tag}\t${token.lemma}\t${token.inflectionForm}\tdep=${token.dep}`);
  }
}
