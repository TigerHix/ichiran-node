import { GrammarEngine } from './src/engine/index.js';
import { compileRuleset } from './src/ruleset.js';
import youou from './src/rules/bunpro/jlpt4/よう-おう.js';

async function main() {
  const ruleset = { id: 'test', rules: [youou] };
  const program = compileRuleset(ruleset);
  const engine = new GrammarEngine(program);
  
  const tests = [
    '運転しよう',
    'かってあげよう',
    'かえろう',
    'まとう',
    'かこう'
  ];
  
  for (const text of tests) {
    try {
      const result = await engine.analyze(text);
      console.log(`\n"${text}":`);
      result.tokens.forEach(t => {
        console.log(`  text="${t.text}" lemma="${t.lemma}" pos=${t.pos} inflectionForm=${t.inflectionForm}`);
      });
    } catch (e) {
      console.log(`\n"${text}": ERROR - ${e.message}`);
    }
  }
}

main().catch(console.error);
