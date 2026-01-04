import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  '机の上に本がある。',           // Regular ある (existence)
  'お金があるから買える。',        // Regular ある
  'ベジタリアン用のメニューあり！',  // Literary あり (possibility)
];

for (const s of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(s);
  console.log('='.repeat(80));
  const doc = await engine.analyze(s);
  if (doc && doc.sentences[0]) {
    for (const tok of doc.sentences[0].tokens) {
      if (tok.text === 'ある' || tok.text === 'あり') {
        console.log(`text=${tok.text} POS=${tok.pos} lemma=${tok.lemma} dep=${tok.dep} inflectionForm=${tok.inflectionForm || 'UNSET'} head=${tok.head}`);
      }
    }
  }
}

await engine.close();
