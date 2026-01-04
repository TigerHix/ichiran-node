import { GinzaClient } from "./src/ginza/client.js";

const client = new GinzaClient();
await client.start();

const sentences = [
  "探していたカバンをみつけた。",
  "台所のライトがきえた。",
  "本当に気温が急にさがりましたね。",
  "犯人がみつかった。",
  "あそこにある窓をあけてください。",
];

for (const text of sentences) {
  console.log("\n=== " + text + " ===");
  const docs = await client.analyze([text]);
  for (const doc of docs) {
    for (const sent of doc.sentences) {
      for (const token of sent.tokens) {
        console.log(`  ${token.text.padEnd(10)} pos:${token.pos.padEnd(6)} lemma:${token.lemma.padEnd(10)} dep:${token.dep}`);
      }
    }
  }
}

await client.stop();
