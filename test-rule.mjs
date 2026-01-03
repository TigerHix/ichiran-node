import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';
import janakatta from './packages/grammar/src/rules/bunpro/jlpt5/じゃなかった.ts';

async function main() {
  const e = await GrammarEngine.create([{ id: 'test', rules: [janakatta] }], {
    ginza: { python: 'python3' },
  });

  const test = 'りんごじゃなかった。';
  console.log('Testing:', test);

  const result = await e.explainMatch(test, 'じゃなかった');
  console.log(JSON.stringify(result, null, 2));

  await e.close();
}

main();
