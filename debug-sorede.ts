import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT4 } from './packages/grammar/src/rules/bunpro/jlpt4/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT4], {
    ginza: { python: 'python3' }
  });

  const sentence = '私は子供のころにお医者さんに命を救われた。それで医者になろうと思った。';
  
  // Get GiNZA parse
  const doc = await engine.ginza.parse(sentence);
  
  console.log('Sentence:', sentence);
  console.log('\nTokens around それで:');
  
  for (let i = 0; i < doc.tokens.length; i++) {
    const t = doc.tokens[i];
    if (t.text.includes('それ') || t.text === 'で') {
      console.log(`  [${i}] "${t.text}" pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
    }
  }
  
  await engine.close();
}

main().catch(console.error);
