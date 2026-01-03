import { GrammarEngine } from '../src/program.js';
import { BUNPRO_JLPT5 } from '../src/rules/bunpro/jlpt5/index.js';
import { loadTestItem } from '../src/rules/bunpro/_test/helpers.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' }
  });

  // Load test data for か-or
  const item = loadTestItem('か-or', 'JLPT5');

  console.log('Positive test cases:');
  for (const { sentence } of item.sentences.slice(0, 10)) {
    const doc = await engine.analyze(sentence);
    if (doc && doc.sentences[0]) {
      const tokens = doc.sentences[0].tokens;
      const kaTokens = tokens.filter(t => t.text === 'か');
      if (kaTokens.length > 0) {
        const ka = kaTokens[0];
        console.log(`  "${sentence}" - か: pos=${ka.pos}, dep=${ka.dep}`);
      }
    }
  }

  await engine.close();
}

main().catch(console.error);
