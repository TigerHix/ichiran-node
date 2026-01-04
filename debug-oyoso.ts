import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT2 } from './packages/grammar/src/rules/bunpro/jlpt2/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT2]);

const sentences = [
  'まずは計画のおおよそを説明しよう。',
  '地震による犠牲者はおおよそ１２０人です。',
  '私はおおよそ理解したけど、周りの子達はポカーンとした表情で先生を見ていた。',
  '美術館では、凡そ高価な作品を展示している。',
];

for (const s of sentences) {
  console.log('===', s, '===');
  const doc = await engine.analyze(s);
  if (doc && doc.sentences.length > 0) {
    for (const t of doc.sentences[0].tokens) {
      console.log(`  ${t.text} | pos=${t.pos} | lemma=${t.lemma} | dep=${t.dep} | head=${t.head}`);
    }
  }
  console.log('');
}

await engine.close();
