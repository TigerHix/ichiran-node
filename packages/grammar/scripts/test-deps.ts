import { GrammarEngine } from '../src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' }
  });

  const sentences = [
    '旅行に行くか休む。',
    '彼は「来るか」と聞いた。',
    '彼が来るかどうか分かりません。',
  ];

  for (const sent of sentences) {
    console.log('\n=== Sentence:', sent, '===');
    const doc = await engine.analyze(sent);
    
    // Find か and と tokens
    for (let i = 0; i < doc.tokens.length; i++) {
      const t = doc.tokens[i];
      if (t.text === 'か' || t.text === 'と' || t.text === 'どう') {
        console.log(`  Token ${i}: "${t.text}" (pos=${t.pos}, dep=${t.dep}, head=${t.head}, lemma=${t.lemma})`);
      }
    }
  }

  await engine.close();
}

main().catch(console.error);
