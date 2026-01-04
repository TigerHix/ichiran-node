import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  '駐車場ありのホテル',
  'お酒もありだけど今日はジュースを飲もうかな。',
  '遊園地もありだ',
  'キッズスペースありだって',
  'ネコもありだ',
  'ラーメンもありじゃない？',
];

for (const s of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(s);
  console.log('='.repeat(80));
  const doc = await engine.analyze(s);
  if (doc && doc.sentences[0]) {
    for (const tok of doc.sentences[0].tokens) {
      console.log(`${tok.text.padEnd(10)} POS=${tok.pos.padEnd(6)} lemma=${tok.lemma.padEnd(10)} dep=${tok.dep.padEnd(8)} inflectionForm=${tok.inflectionForm || 'N/A'} head=${tok.head}`);
    }
  }
}

await engine.close();
