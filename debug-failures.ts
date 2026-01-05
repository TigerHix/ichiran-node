import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([]);

  const sentences = [
    '当社は事情のいかんにかかわらず、責任を負いません。',
    '成績のいかんにかかわらず、不合格となります。',
    '内容のいかんにかかわらずお答えできません。',
    '結果いかんをとわず、褒めています。',
  ];

  for (const sentence of sentences) {
    const doc = await engine.analyze(sentence);
    
    console.log('\n' + sentence);
    console.log('='.repeat(80));
    doc.sentences[0].tokens.forEach((t: any) => {
      console.log(`[${t.i}] "${t.text}" lemma="${t.lemma}" pos=${t.pos} dep=${t.dep}`);
    });
  }
}

main().catch(console.error);
