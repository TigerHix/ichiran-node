import { GrammarEngine } from './src/index.js';

async function debug() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });
  
  const sentences = [
    'マサミは綺麗で水泳が趣味です。',
    'シンプルで便利なスマホを買うつもりだよ。',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(60));
    console.log(`Sentence: ${sentence}`);
    const doc = await engine.analyze(sentence);
    
    // Find "で" token and show context
    for (const token of doc.sentences[0].tokens) {
      if (token.text === 'で') {
        console.log(`Token ${token.i}: "${token.text}" (pos=${token.pos}, tag=${token.tag}, lemma=${token.lemma}, dep=${token.dep})`);
        // Show previous and next tokens
        if (token.i > 0) {
          const prev = doc.sentences[0].tokens[token.i - 1];
          console.log(`  Previous: "${prev.text}" (pos=${prev.pos}, tag=${prev.tag}, lemma=${prev.lemma}, dep=${prev.dep})`);
        }
        if (token.i < doc.sentences[0].tokens.length - 1) {
          const next = doc.sentences[0].tokens[token.i + 1];
          console.log(`  Next: "${next.text}" (pos=${next.pos}, tag=${next.tag}, lemma=${next.lemma}, dep=${next.dep})`);
        }
      }
    }
  }
  
  await engine.close();
}

debug();
