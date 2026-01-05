import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([]);

  const sentences = [
    '理由のいかんによらず、自由に返品することが可能です。',
    '天候のいかんにかかわらず、明日のコンサートは予定通りに開催されます。',
    '結果いかんをとわず、この学校では努力をした生徒を褒めるようにしています。',
    '理由のいかんを問わず提出期限を過ぎたレポートは受け取らないつもりだ',
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
