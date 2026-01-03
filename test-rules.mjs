import { GrammarEngine } from './packages/grammar/src/program.js';
import janakatta from './packages/grammar/src/rules/bunpro/jlpt5/じゃなかった.ts';

async function main() {
  const e = await GrammarEngine.create([{ id: 'test', rules: [janakatta] }], {
    ginza: { python: 'python3' },
  });

  const tests = [
    'りんごじゃなかった。',
    'あれは猫ではありませんでした。',
    'この車は便利じゃなかった。',
  ];

  for (const test of tests) {
    console.log('\n=== ' + test + ' ===');
    const result = await e.explainMatch(test, 'じゃなかった');
    console.log('Matched:', result.matched);
    if (!result.matched) {
      console.log('Reason:', result.reason);
      console.log('Partial bindings:', JSON.stringify(result.partialBinding));
    }
  }

  await e.close();
}

main();
