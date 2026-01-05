import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([]);

  const sentence = '結果いかんをとわず、この学校では努力をした生徒を褒めるようにしています。';
  const doc = await engine.analyze(sentence);
  
  console.log('Sentence:', sentence);
  console.log('='.repeat(80));
  doc.sentences[0].tokens.forEach((t: any) => {
    console.log(`[${t.i}] "${t.text}" lemma="${t.lemma}" pos=${t.pos} tag=${t.tag}`);
  });
}

main().catch(console.error);
