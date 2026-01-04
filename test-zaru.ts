import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const client = new GinzaClient();
await client.start();

const sentences = [
  '知られざる傑作',
  '絶えざる失敗',
  '言わざる人',
  'たゆまざる努力',
  '消えざる傷',
];

for (const sentence of sentences) {
  console.log('\n=== ' + sentence + ' ===');
  const docs = await client.analyze([sentence]);
  console.log(JSON.stringify(docs[0], null, 2));
}

await client.stop();
