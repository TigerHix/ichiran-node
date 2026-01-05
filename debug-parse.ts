import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([]);

  const sentences = [
    '結果いかんをとわず、この学校では努力をした生徒を褒めるようにしています。',
    'テストの形式いかんをとわず自分の力を最大限出せるように勉強しよう。',
    '理由のいかんによらず、自由に返品することが可能です。',
    '天候のいかんにかかわらず、明日のコンサートは予定通りに開催されます。',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`Sentence: ${sentence}`);
    console.log('='.repeat(80));
    const doc = await engine.analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }
}

main().catch(console.error);
