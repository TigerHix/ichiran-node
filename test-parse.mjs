import { createEngine } from './packages/grammar/src/engine/compiler.js';
import { loadRuleset } from './packages/grammar/src/ruleset.js';

async function test() {
  const ruleset = await loadRuleset('./packages/grammar/src/rules/bunpro/jlpt5/index.ts');
  const engine = await createEngine(ruleset);

  const sentences = [
    '寒かった。',
    '寒かったです。',
    '楽しかった。',
    '楽しかったです。',
    'よかった。',
    'よかったです。',
    '寒いです。',
    'たのしいです。',
    '静かだった。',  // na-adjective past - should NOT match
    '食べた。',      // verb past - should NOT match
  ];

  for (const sentence of sentences) {
    console.log('\n=== ' + sentence + ' ===');
    const doc = await engine.analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }
}

test().catch(console.error);
