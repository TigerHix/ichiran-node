// Test to check GiNZA parsing for noun + がち patterns
import { GrammarEngine } from './src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });
  
  const sentences = [
    '彼はいつも遠慮がちだ。',
    '私の息子は病気がちだ。',
    'うちの子は病気がちなので、週に二、三日ぐらいは学校を休みます。',
    '恥ずかしがり屋だからなのか、彼女はいつも伏し目がちです。',
  ];
  
  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const doc = await engine.analyze(sent);
    if (!doc) {
      console.log('  ERROR: doc is null');
      continue;
    }
    for (const sentence of doc.sentences) {
      sentence.tokens.forEach((t, i) => {
        console.log(`  ${i}: ${t.text} (${t.pos}, ${t.lemma}, tag=${t.tag}, inf=${t.inflectionForm}, dep=${t.dep})`);
      });
    }
  }
  
  await engine.close();
}

main().catch(console.error);
