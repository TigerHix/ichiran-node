import { GrammarEngine } from './packages/grammar/src/program.js';
import ariRule from './packages/grammar/src/rules/bunpro/jlpt3/あり.js';

const engine = await GrammarEngine.create([{ id: 'test', rules: [ariRule] }], {
  ginza: { python: 'python3' },
});

const negatives = [
  '机の上に本がある。',
  'お金があるから買える。',
  '彼は才能がある。',
  '昨日は雨があった。',
  '名前があって面白い。',
  'ある日、男が来た。',
  '部屋には誰もありません。',
  '昨日は雨がありませんでした。',
  '食堂があります。',
];

console.log('Testing negative cases (should NOT match):\n');
for (const s of negatives) {
  const result = await engine.explainMatch(s, 'あり');
  if (result.matched) {
    console.log(`❌ FALSE POSITIVE: ${s}`);
    console.log(`   Captures:`, result.captures);
  } else {
    console.log(`✓ Correctly rejected: ${s}`);
  }
}

await engine.close();
