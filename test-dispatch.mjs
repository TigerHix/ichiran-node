import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.ts';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  // Check the dispatch map
  console.log('Dispatch map for "text:じゃ":', e.program.dispatch.get('text:じゃ'));
  console.log('Dispatch map for "lemma:だ":', e.program.dispatch.get('lemma:だ'));

  await e.close();
}

main();
