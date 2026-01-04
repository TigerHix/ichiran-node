import { GinzaClient } from './packages/grammar/src/ginza/client.js';

async function main() {
  const client = await GinzaClient.create();
  
  const sentences = [
    '私は一ヶ月に一回友達と遊園地に行く。',
    '公務員は一年に一回ボーナスをもらう。',
  ];
  
  for (const sentence of sentences) {
    console.log('\n=== ' + sentence + ' ===\n');
    const doc = await client.analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }
  
  await client.stop();
}

main().catch(console.error);
