import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.ts';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  const test = 'りんごじゃなかった。';
  console.log('Testing:', test);

  const result = await e.explainMatch(test, 'じゃなかった');
  console.log('Matched:', result.matched);
  if (!result.matched) {
    console.log('Reason:', result.reason);
    console.log('Partial bindings:', JSON.stringify(result.partialBinding));
  }

  await e.close();
}

main();
