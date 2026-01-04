<<<<<<< HEAD
import { GinzaClient } from "./src/ginza/client.js";

const client = new GinzaClient();
await client.start();

const sentences = [
  "文化祭の準備をしている生徒：「だいたいでいいから、午前中までにはおわらせておいて。」",
  "だいたいでいいから、午前中までにはおわらせておいて。",
];

for (const text of sentences) {
  console.log("\n=== " + text.substring(0, 50) + " ===");
  const docs = await client.analyze([text]);
  for (const doc of docs) {
    for (const sent of doc.sentences) {
      for (const token of sent.tokens) {
        if (token.text.includes('だいたい') || token.text === 'だいたい') {
          console.log("  FOUND:", token.text, "pos:", token.pos, "lemma:", token.lemma);
        }
      }
    }
  }
}

await client.stop();
=======
// Simple script to check GiNZA parses without importing complex modules
const testSentences = [
  '勉強しようとしたが、疲れていた。',
  '逃げようとしたが、警察に捕まった。',
];

// The test sentences from Bunpro have the grammar point embedded
// Let's extract the full sentences with the grammar point
>>>>>>> jlpt3-verb-volitional-としたが
