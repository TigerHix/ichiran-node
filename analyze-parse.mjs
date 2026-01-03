import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  const sentences = [
    'その動物は、牛じゃなかった。',
    'あれは猫ではありませんでした。',
    'この車は便利じゃなかった。',
    '晩ごはんが好きではなかった。',
    '宿題は大変ではありませんでした。',
  ];

  for (const s of sentences) {
    console.log('\n=== ' + s + ' ===');
    const doc = await e.analyze(s);
    console.log(JSON.stringify(doc, null, 2));
  }

  await e.close();
}

main();
