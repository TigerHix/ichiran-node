import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT4 } from './packages/grammar/src/rules/bunpro/jlpt4/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT4], {
  ginza: { python: 'python3' }
});

const sentences = [
  'どのスポーツが好きですか。',
  'どのパソコンがいいですか。',
  'どのレストランに行く？',
];

for (const sent of sentences) {
  console.log(`\n=== ${sent} ===`);
  const doc = await engine.analyze(sent);
  if (doc && doc.sentences[0]) {
    for (const tok of doc.sentences[0].tokens) {
      console.log(`  ${tok.text}: pos=${tok.pos}, lemma=${tok.lemma}, dep=${tok.dep}, tag=${tok.tag || 'none'}, inflectionForm=${tok.inflectionForm || 'none'}`);
    }
  }
}

await engine.close();
