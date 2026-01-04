// Quick debug script to see how GiNZA parses this sentence
import { startWorker } from './packages/grammar/src/ginza/client.js';

async function main() {
  const client = await startWorker();
  
  const sentence = 'まずは計画のおおよそを説明しよう。';
  const result = await client.parse(sentence);
  
  console.log('Sentence:', sentence);
  console.log('\nTokens:');
  for (const s of result.docs[0].sentences) {
    for (const t of s.tokens) {
      const text = t.text.padEnd(15);
      const lemma = t.lemma.padEnd(15);
      const pos = t.pos.padEnd(8);
      const dep = t.dep.padEnd(8);
      console.log(`Text: ${text} Lemma: ${lemma} POS: ${pos} Dep: ${dep} Head: ${t.head}`);
    }
  }
  
  await client.stop();
}

main().catch(console.error);
