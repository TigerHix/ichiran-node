import { GrammarEngine } from '../src/program.js';
import rule from '../src/rules/bunpro/jlpt5/か-or.js';
import { BUNPRO_JLPT5 } from '../src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' }
  });

  const sentences = [
    '旅行に行くか休む。',
    '彼は「来るか」と聞いた。',
    '彼が来るかどうか分かりません。',
  ];

  for (const sent of sentences) {
    console.log('\n=== Sentence:', sent, '===');
    const explain = await engine.explainMatch(sent, rule.id);
    console.log('Matched:', explain.matched);
    if (!explain.matched) {
      console.log('  Reason:', explain.reason);
      if (explain.failedClause) {
        console.log('  Failed clause:', explain.failedClause);
      }
      if (explain.partialBinding) {
        console.log('  Partial binding:', explain.partialBinding);
      }
    }
  }

  await engine.close();
}

main().catch(console.error);
