import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([]);

  const sentence = '理由のいかんによらず、自由に返品することが可能です。';
  const doc = await engine.analyze(sentence);
  
  console.log('Tokens for:', sentence);
  console.log('='.repeat(80));
  doc.sentences[0].tokens.forEach((t: any) => {
    console.log(`[${t.i}] "${t.text}" lemma="${t.lemma}" pos=${t.pos} dep=${t.dep} head=${t.head}`);
  });
}

main().catch(console.error);
