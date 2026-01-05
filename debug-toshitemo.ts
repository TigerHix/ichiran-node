import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT2 } from './packages/grammar/src/rules/bunpro/jlpt2/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT2]);

const sentences = [
  '大人向きの本だとしても売れます。',
  'たとえできるとしても、やらないだろう。',
  '彼はピアノ奏者だが、オルガン奏者としても有名です。',
];

for (const sent of sentences) {
  console.log('\n=== ' + sent + ' ===');
  const doc = await engine.analyze(sent);
  console.log('doc:', JSON.stringify(doc, null, 2));
  if (doc && doc.sentences && doc.sentences[0]) {
    for (const token of doc.sentences[0].tokens) {
      console.log(`  ${token.text}: pos=${token.pos}, lemma=${token.lemma}, inflectionForm=${token.inflectionForm}`);
    }
  }
}
