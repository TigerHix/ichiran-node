import { GrammarEngine } from './packages/grammar/src/program.js';

async function debug() {
  const engine = await GrammarEngine.create([], { ginza: { python: 'python3' } });

  const sentences = [
    'いくらしんせつにしても、あの人は全然感謝しない人だ。',
    'いくら優秀でも、性格が悪い人とは一緒に仕事をしたくありません。',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`SENTENCE: ${sentence}`);
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
