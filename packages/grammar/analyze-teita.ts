import { GrammarEngine } from './src/program.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT4]);

  const sentences = [
    'また僕が勉強していた時邪魔したね。',
    '待っていなかった。',
  ];

  for (const sentence of sentences) {
    console.log(`\n=== ${sentence} ===`);
    const doc = await engine.analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }

  await engine.close();
}

main();
