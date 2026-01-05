import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create();

  const sentences = [
    'ことわるにことわれない',
    'ことわるにことわれません',
    'なくになけない',
    'わらうにわらえない',
    'にげるににげられない',
    'ねるにねられない',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(60));
    console.log('Sentence:', sentence);
    console.log('='.repeat(60));

    const doc = await engine.analyze(sentence);
    for (let i = 0; i < doc.tokens.length; i++) {
      const tok = doc.tokens[i];
      console.log(
        `[${i}] ${tok.text.padEnd(10)} ` +
        `POS=${tok.pos.padEnd(6)} ` +
        `lemma=${tok.lemma.padEnd(10)} ` +
        `inf=${tok.inflectionForm || '-'.padEnd(15)} ` +
        `conj=${tok.conjugationClass || '-'} ` +
        `dep=${tok.dep || '-'}`
      );
    }
  }
}

main().catch(console.error);
