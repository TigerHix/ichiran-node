import { GrammarEngine } from './packages/grammar/src/program.ts';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.ts';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  async function analyze(sentence) {
    const doc = await engine.analyze(sentence);
    if (!doc) return;
    console.log('\n' + sentence + ':');
    doc.tokens.forEach(t => {
      console.log('  ', t.text, 'pos=' + t.pos, 'tag=' + (t.tag || 'N/A'), 'lemma=' + t.lemma, 'dep=' + (t.dep || 'N/A'), 'head=' + t.head);
    });
  }

  await analyze('おいしいピザを食べる。');
  await analyze('怖い先生。');
  await analyze('速い車。');
  await analyze('かっこいい先輩。');

  await engine.close();
}

main().catch(console.error);
