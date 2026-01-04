import { createEngine } from './packages/grammar/src/engine/index.ts';

const engine = await createEngine([]);

// らしい2 examples (characteristic of)
const characteristicExamples = [
  'トムは本当に男らしいね。',
  '今日は８月らしくないです。',
  '彼女は女らしくない人だ。',
  'やっと夏らしい日が来たよ！',
  'そんなことを言うのは、彼女らしい。',
  'テーマパークらしいテーマパークの方が好きだよ。',
  'もっと大人らしくしなさいよ！',
  'カップルらしく映画を見に行こう！',
];

// らしい1 examples (hearsay)
const hearsayExamples = [
  '雨が降るらしい。',
  '彼は先生らしい。',
  '明日は雪らしい。',
  '田中さんは来ないらしい。',
];

console.log('=== CHARACTERISTIC (らしい2) ===');
for (const sent of characteristicExamples) {
  console.log(`\n${sent}`);
  const doc = await engine.analyze(sent);
  doc.tokens.forEach(t => {
    if (t.lemma?.includes('らしい') || t.text?.includes('らしい')) {
      console.log(`  ${t.text} [pos=${t.pos}, lemma=${t.lemma}, dep=${t.dep}, head=${t.head}]`);
    }
  });
}

console.log('\n\n=== HEARSAY (らしい1) ===');
for (const sent of hearsayExamples) {
  console.log(`\n${sent}`);
  const doc = await engine.analyze(sent);
  doc.tokens.forEach(t => {
    if (t.lemma?.includes('らしい') || t.text?.includes('らしい')) {
      console.log(`  ${t.text} [pos=${t.pos}, lemma=${t.lemma}, dep=${t.dep}, head=${t.head}]`);
    }
  });
}
