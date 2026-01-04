import { GrammarEngine } from './src/program.js';
import { BUNPRO_JLPT5 } from './src/rules/bunpro/jlpt5/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT5]);

const sentences = [
  '焼きそばでもいいですか？',
  '食べてもいい。',
  'これ食べてもいいの？',
];

for (const s of sentences) {
  console.log('===', s, '===');
  const doc = await engine.analyze(s);
  if (doc && doc.sentences.length > 0) {
    for (const t of doc.sentences[0].tokens) {
      console.log(`  ${t.text} | pos=${t.pos} | lemma=${t.lemma} | dep=${t.dep} | head=${t.head} | inflectionForm=${t.inflectionForm}`);
    }
  }
  console.log('');
}

await engine.close();
