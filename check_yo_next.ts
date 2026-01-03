import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5]);
  
  const sentences = [
    '今日は水曜日だよ。',
    '四月よふみは誰ですか。',
  ];
  
  for (const sentence of sentences) {
    console.log('\n=== ' + sentence + ' ===');
    const doc = await engine.analyze(sentence);
    if (doc && doc.sentences[0]) {
      const sent = doc.sentences[0];
      const yoIndex = sent.tokens.findIndex(t => t.text === 'よ');
      if (yoIndex >= 0) {
        console.log(`よ is at index ${yoIndex}`);
        if (yoIndex + 1 < sent.tokens.length) {
          const next = sent.tokens[yoIndex + 1];
          console.log(`Next token: text="${next.text}" pos="${next.pos}" dep="${next.dep}"`);
        } else {
          console.log(`よ is the last token`);
        }
      }
    }
  }
  
  await engine.close();
}

main().catch(console.error);
