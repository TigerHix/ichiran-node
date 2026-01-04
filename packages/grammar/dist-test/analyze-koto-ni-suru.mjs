import { GrammarEngine } from './src/program.js';

const sentences = [
  '一本早い電車に乗ることにした。',
  'タマキさんと箱根に行くことにする。',
  'テレビゲームをしないことにします。',
  '肉を食べることにする。',
  '肉を食べないことにする。',
  '毎日野菜ジュースを飲む事にしている。',
];

const engine = await GrammarEngine.create([], { client: globalThis.__sharedGinzaClient });

for (const sent of sentences) {
  console.log('\n=== ' + sent + ' ===');
  const doc = await engine.analyze(sent);
  doc.tokens.forEach((t, i) => {
    console.log(`${i}: ${t.text} [${t.pos}] lemma=${t.lemma} infl=${t.inflectionForm}`);
  });
}
