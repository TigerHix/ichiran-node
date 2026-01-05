import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT2 } from './packages/grammar/src/rules/bunpro/jlpt2/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT2]);

const sent = '大人向きの本だとしても売れます。';

console.log('=== ' + sent + ' ===');
const doc = await engine.analyze(sent);

if (doc && doc.sentences && doc.sentences[0]) {
  const tokens = doc.sentences[0].tokens;

  // Find tokens containing して
  for (const token of tokens) {
    if (token.text.includes('して') || token.text.includes('し')) {
      console.log(`\nToken with "${token.text}":`);
      console.log(`  text: ${token.text}`);
      console.log(`  lemma: ${token.lemma}`);
      console.log(`  pos: ${token.pos}`);
      console.log(`  inflectionForm: ${token.inflectionForm}`);
      console.log(`  dep: ${token.dep}`);
      console.log(`  head: ${token.head}`);
    }
  }

  console.log('\n\nAll tokens:');
  for (const token of tokens) {
    console.log(`  ${token.text}: lemma=${token.lemma}, pos=${token.pos}, inflectionForm=${token.inflectionForm}`);
  }
}
