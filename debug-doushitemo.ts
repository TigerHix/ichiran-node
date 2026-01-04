import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const sentences = [
  'どうしても諦められない夢がある。',
  '食べるとどうしても眠たくなる。',
  'どうしてもアイスクリームが食べたい。',
  'この癖はどうしても治らない。',
];

const ginza = new GinzaClient();

for (const sentence of sentences) {
  console.log('\n=== ' + sentence + ' ===');
  const docs = await ginza.analyze([sentence]);
  console.log('DOCS:', JSON.stringify(docs, null, 2));
}
