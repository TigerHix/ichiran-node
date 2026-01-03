import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.ts';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  const test = 'りんごじゃなかった。';
  console.log('Testing:', test);

  const hits = await e.match(test);
  console.log('All hits:', JSON.stringify(hits, null, 2));

  const janakattaHit = hits.find((h) => h.ruleId === 'じゃなかった');
  console.log('\nじゃなかった hit:', janakattaHit ? 'FOUND' : 'NOT FOUND');

  await e.close();
}

main();
