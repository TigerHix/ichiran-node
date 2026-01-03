import { GrammarEngine } from '../src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' }
  });

  const sentences = [
    { sent: '旅行に行くか休む。', label: 'positive' },
    { sent: '彼は「来るか」と聞いた。', label: 'quotation' },
    { sent: '彼が来るかどうか分かりません。', label: 'indirect' },
  ];

  for (const { sent, label } of sentences) {
    console.log('\n===', label, ':', sent, '===');
    const doc = await engine.analyze(sent);
    if (!doc) continue;

    for (const sentence of doc.sentences) {
      sentence.tokens.forEach((t, i) => {
        console.log(`  ${i}: "${t.text}" (${t.pos})`);
      });
    }
  }

  await engine.close();
}

main().catch(console.error);
