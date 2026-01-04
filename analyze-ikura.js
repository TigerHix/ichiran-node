// Script to analyze GiNZA parses for いくら-でも pattern
const sentences = [
  'いくら言っても、何も変わらないよ。',
  'いくら急いでも土曜日までには終わらない。',
  'いくら新しくても落としたら壊れるに決まってるじゃん。',
  'いくら社長でも、あんな言い方をしてはいけません。',
  'いくら優秀でも、性格が悪い人とは一緒に仕事をしたくありません。',
  '相談ならいくらでも聞いてあげるよ。',
  'お菓子ならいくらでもあるから、好きなだけ持っていきな！',
];

async function analyze() {
  const { GrammarEngine } = await import('./packages/grammar/dist/program.js');
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`SENTENCE: ${sentence}`);
    console.log('='.repeat(80));
    const doc = await engine.analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }

  await engine.close();
}

analyze().catch(console.error);
