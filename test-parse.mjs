import { GrammarEngine } from './packages/grammar/src/program.js';

async function test() {
  const ruleset = { id: 'test', rules: [] };
  const engine = await GrammarEngine.create([ruleset]);

  const testCases = [
    '今、映画を見ているところです',
    '仕事をしているところだから',
    '今水道管を直してもらっているところだから',
  ];

  for (const sentence of testCases) {
    console.log('\n===', sentence, '===');
    const doc = await engine.analyze(sentence);
    if (doc && doc.tokens) {
      doc.tokens.forEach((t, i) => {
        console.log(`  ${i}: ${t.text} (pos=${t.pos}, lemma=${t.lemma}, dep=${t.dep}, head=${t.head}, form=${t.inflectionForm})`);
      });
    }
  }
}

test().catch(console.error);
