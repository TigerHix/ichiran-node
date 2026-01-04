import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = new GrammarEngine({ rulesets: [] });

const testCases = [
  'くやしげな表情',
  '不安げに手を上げました',
  '楽しげに話している',
  'まんぞくげなの',
];

for (const text of testCases) {
  console.log('\n=== ' + text + ' ===');
  const result = await engine.analyze(text);
  result.tokens.forEach((t, i) => {
    console.log(`${i}: "${t.text}" POS=${t.pos} tag=${t.tag} lemma=${t.lemma}`);
  });
}
