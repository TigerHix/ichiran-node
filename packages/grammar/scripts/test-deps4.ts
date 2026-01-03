import { GrammarEngine } from '../src/program.js';

async function main() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' }
  });

  const sentences = [
    { sent: '旅行に行くか休む。', label: 'positive' },
    { sent: '彼が来るかどうか分かりません。', label: 'indirect' },
  ];

  for (const { sent, label } of sentences) {
    console.log('\n===', label, ':', sent, '===');
    const doc = await engine.analyze(sent);
    if (!doc) continue;

    for (const sentence of doc.sentences) {
      // Find all か particles
      const kaTokens = [];
      for (let i = 0; i < sentence.tokens.length; i++) {
        if (sentence.tokens[i].text === 'か' && sentence.tokens[i].pos === 'PART') {
          kaTokens.push(i);
        }
      }

      for (const kaIdx of kaTokens) {
        const ka = sentence.tokens[kaIdx];
        const head = sentence.tokens[ka.head];
        console.log(`\n  か at index ${kaIdx}:`);
        console.log(`    dep=${ka.dep}`);
        console.log(`    head="${head.text}" (idx=${ka.head}, dep=${head.dep})`);

        // Check what comes after the head
        const afterHead = sentence.tokens.slice(ka.head + 1, ka.head + 4);
        console.log(`    tokens after head:`, afterHead.map(t => `${t.text}(${t.pos})`).join(', '));
      }
    }
  }

  await engine.close();
}

main().catch(console.error);
