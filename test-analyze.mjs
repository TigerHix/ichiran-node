import { createEngine } from './packages/grammar/src/engine/index.js';

const engine = await createEngine();

const sentences = [
  '妹が孫のように甘える。',
  'スープのようなカレー。',
  'ウサインボルトのように走るのが夢です。',
  'この先生は鬼のように怖い。',
  'このような洋服を探しています。',
  '今はスマートフォンのような携帯がたくさんあります。',
];

for (const sentence of sentences) {
  console.log('\n=== ' + sentence + ' ===');
  const doc = await engine.analyze(sentence);
  for (const token of doc.tokens) {
    const tag = token.tag || 'N/A';
    const inf = token.inflectionForm || 'N/A';
    console.log(`  ${token.text}: pos=${token.pos}, lemma=${token.lemma}, tag=${tag}, dep=${token.dep}, inflectionForm=${inf}`);
  }
}
