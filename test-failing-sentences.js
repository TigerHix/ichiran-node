import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  '駐車場ありのホテルを取っておいてください。',
  '字幕ありで見たいから字幕つけてもらえない？',
  'キッズメニューありのレストランに行こう。',
  'この飲食店はキッズスペースありだって！珍しいね！',
  'ベジタリアン用のメニューあり！',
];

for (const s of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(s);
  console.log('='.repeat(80));
  const doc = await engine.analyze(s);
  if (doc && doc.sentences[0]) {
    for (const tok of doc.sentences[0].tokens) {
      if (tok.text === 'あり') {
        console.log(`✓ あり: text=${tok.text} POS=${tok.pos} lemma=${tok.lemma} dep=${tok.dep} inflectionForm=${tok.inflectionForm || 'UNSET'} head=${tok.head}`);
      }
    }
  }
}

await engine.close();
