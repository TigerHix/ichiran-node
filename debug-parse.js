import { GrammarEngine } from './packages/grammar/src/program.js';

async function debug() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });

  const sentences = [
    'いくらお金があっても、愛は買えない。',
    '文プロを見つけるまでいくら文法を勉強しても、理解ができなかった。',
    'いくら親切にしても、あの人は全然感謝しない人だ。',
    'いくら忠告しても、彼は考えを変える気がないらしい。',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`SENTENCE: ${sentence}`);
    console.log('='.repeat(80));
    const doc = await engine.analyze(sentence);

    for (const sent of doc.sentences) {
      for (const tok of sent.tokens) {
        console.log(`[${tok.i}] ${tok.text} (lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, infl=${tok.inflectionForm}) [head=${tok.head}]`);
      }
    }
  }

  await engine.close();
}

debug().catch(console.error);
