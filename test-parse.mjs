import { GrammarEngine } from './packages/grammar/src/engine/compiler.ts';

async function main() {
  const engine = new GrammarEngine([]);
  
  const tests = [
    '心身と共に健康だ。',
    '風と共に去りぬ。',
  ];

  for (const text of tests) {
    console.log('\n=== ' + text + ' ===');
    const doc = await engine.analyze(text);
    for (let i = 0; i < doc.tokens.length; i++) {
      const t = doc.tokens[i];
      console.log(`${i}: "${t.text}" (pos=${t.pos}, lemma="${t.lemma}", tag="${t.tag || 'N/A'}")`);
    }
  }
}

main().catch(console.error);
