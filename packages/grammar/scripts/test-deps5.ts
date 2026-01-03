import { GrammarEngine } from '../src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' }
  });

  const sentences = [
    { sent: '旅行に行くか休む。', label: 'positive' },
    { sent: '彼は「来るか」と聞いた。', label: 'quotation' },
  ];

  for (const { sent, label } of sentences) {
    console.log('\n=== ' + label + ' : ' + sent + ' ===');
    const doc = await engine.analyze(sent);
    if (!doc) continue;

    for (const sentence of doc.sentences) {
      sentence.tokens.forEach((t, i) => {
        if (t.text === 'か' || t.text === 'と') {
          const head = sentence.tokens[t.head];
          console.log('  ' + t.text + ' (idx=' + i + ', dep=' + t.dep + ') -> head="' + head.text + '" (idx=' + t.head + ')');
        }
      });
    }
  }

  await engine.close();
}

main().catch(console.error);
