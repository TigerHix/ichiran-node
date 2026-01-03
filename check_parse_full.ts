import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5]);
  
  const sentence = '四月よふみは誰ですか。';
  console.log('Original text: ' + sentence);
  const doc = await engine.analyze(sentence);
  if (doc) {
    console.log('Doc text: ' + doc.text);
    console.log('Number of sentences: ' + doc.sentences.length);
    for (let s = 0; s < doc.sentences.length; s++) {
      const sent = doc.sentences[s];
      console.log(`\nSentence ${s}:`);
      for (let i = 0; i < sent.tokens.length; i++) {
        const t = sent.tokens[i];
        console.log(`  [${i}] text="${t.text}" lemma="${t.lemma}" pos="${t.pos}" dep="${t.dep}" head=${t.head}`);
      }
    }
  }
  
  await engine.close();
}

main().catch(console.error);
