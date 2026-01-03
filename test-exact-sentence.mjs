import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.ts';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  const test = '朝ごはんを作ったのは、お父さんじゃありませんでした。';
  console.log('Testing:', test);

  const hits = await e.match(test);
  console.log('Number of hits:', hits.length);
  hits.forEach(h => {
    console.log(`  - ${h.ruleId}: ${JSON.stringify(h.captures)}`);
  });

  const janakattaHit = hits.find((h) => h.ruleId === 'じゃなかった');
  console.log('\nじゃなかった hit:', janakattaHit ? 'FOUND' : 'NOT FOUND');

  // Try explainMatch
  const explain = await e.explainMatch(test, 'じゃなかった');
  console.log('\nexplainMatch:', JSON.stringify(explain, null, 2));

  await e.close();
}

main();
