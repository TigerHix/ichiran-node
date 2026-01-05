import { analyze } from './packages/grammar/src/rules/bunpro/_test/engine.js';

async function main() {
  const testSentences = [
    '毎年家族ぐるみで初詣に行きます。',
    '会社ぐるみで違法な取引をしていたので、社員全員逮捕された。',
    '長年、家族ぐるみで親しくしている友達だよ。',
    '街ぐるみの年中行事「ゴミ拾い」が10月10日に行われます。',
  ];

  for (const sentence of testSentences) {
    console.log(`\n=== ${sentence} ===`);
    const doc = await analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }
}

main();
