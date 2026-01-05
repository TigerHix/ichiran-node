import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create();

  const sentences = [
    '断るに断れない',
    '笑うに笑えない',
    '行くに行けない',
  ];

  for (const sentence of sentences) {
    console.log('\n===', sentence, '===');
    const doc = await engine.analyze(sentence);
    for (const tok of doc.tokens) {
      console.log(`${tok.text}\t${tok.pos}\t${tok.lemma}\t${tok.inflectionForm || '-'}\t${tok.conjugationClass || '-'}`);
    }
  }
}

main().catch(console.error);
