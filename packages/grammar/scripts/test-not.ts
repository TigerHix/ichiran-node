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
    const hits = await engine.match(sent);
    const hit = hits.find((h) => h.ruleId === rule.id);
    console.log('Match:', hit ? 'YES' : 'NO');
    if (hit) {
      console.log('  Captured:', JSON.stringify(hit.captures));
    }
  }

  await engine.close();
}

main().catch(console.error);
