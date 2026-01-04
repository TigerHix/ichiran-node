import { GrammarEngine } from './packages/grammar/src/index.js';

async function debug() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });

  const sentence = '見てたくせに、なんで「見てない」って嘘をつくの？';
  console.log(`Sentence: ${sentence}`);
  const doc = await engine.analyze(sentence);

  console.log('\nAll tokens:');
  for (const token of doc.sentences[0].tokens) {
    console.log(`  ${token.i}: "${token.text}" (pos=${token.pos}, lemma=${token.lemma}, dep=${token.dep}, head=${token.head})`);
  }

  console.log('\nTokens containing "くせ":');
  for (const token of doc.sentences[0].tokens) {
    if (token.text.includes('くせ') || (token.lemma && token.lemma.includes('くせ'))) {
      console.log(`  ${token.i}: "${token.text}" (pos=${token.pos}, lemma=${token.lemma}, dep=${token.dep}, head=${token.head})`);
    }
  }

  await engine.close();
}

debug().catch(console.error);
