import { GrammarEngine } from './packages/grammar/src/index.js';

async function debug() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });

  const testSentences = [
    'コンピュータは以前に比べてずっと使いやすくなった。',
    'ずっと好きです。',
    'ずっと一緒にいる。',
    '点数が伸びれば、あのチームは合格ラインに到達するそうだ。',
  ];

  for (const sentence of testSentences) {
    console.log('\n=== ' + sentence + ' ===');
    const doc = await engine.analyze(sentence);
    for (const token of doc.sentences[0].tokens) {
      console.log(`  ${token.i}: "${token.text}" (pos=${token.pos}, tag=${token.tag}, lemma=${token.lemma}, dep=${token.dep}, head=${token.head})`);
    }
  }

  await engine.close();
}

debug().catch(console.error);
