import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  const sentences = [
    '昨日は、日曜日ではなかった。',
    '晩ごはんが好きではなかった。',
  ];

  for (const s of sentences) {
    console.log('\n=== ' + s + ' ===');
    const doc = await e.analyze(s);
    console.log(JSON.stringify(doc, null, 2));
  }

  await e.close();
}

main();
