import { analyze } from './src/ginza/client.js';

const sentences = [
  "ちょっとまって。",
  "部屋を片付けてね。",
  "助けて！",
  "落ち着いて。",
];

for (const sent of sentences) {
  console.log("\n=== " + sent + " ===");
  const doc = await analyze(sent);
  console.log(JSON.stringify(doc, null, 2));
}
