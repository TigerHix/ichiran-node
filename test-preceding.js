import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  'ベジタリアン用のメニューあり！',
  '字幕ありで見たいから字幕つけてもらえない？',
  'キッズスペースありだって！',
  'ネコもありだ',
];

for (const s of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(s);
  console.log('='.repeat(80));
  const doc = await engine.analyze(s);
  if (doc && doc.sentences[0]) {
    const ariIdx = doc.sentences[0].tokens.findIndex(t => t.text === 'あり');
    if (ariIdx >= 0) {
      // Show tokens around あり
      for (let i = Math.max(0, ariIdx - 2); i <= Math.min(doc.sentences[0].tokens.length - 1, ariIdx + 2); i++) {
        const tok = doc.sentences[0].tokens[i];
        console.log(`${i === ariIdx ? '→' : ' '} ${tok.text.padEnd(10)} POS=${tok.pos.padEnd(6)} lemma=${tok.lemma.padEnd(10)} dep=${tok.dep.padEnd(8)} head=${tok.head}`);
      }
    }
  }
}

await engine.close();
