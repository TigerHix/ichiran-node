import { GrammarEngine } from './packages/grammar/src/program.js';
import rule from './packages/grammar/src/rules/bunpro/jlpt2/ていては.ts';

async function main() {
  const engine = await GrammarEngine.create([{ id: 'test', rules: [rule] }]);
  
  const sent = 'そんな歩き方をしていては、ペンギンかと思われますよ。';
  console.log('===', sent, '===\n');
  
  const doc = await engine.analyze(sent);
  for (let i = 0; i < 15; i++) {
    if (doc.tokens[i]) {
      const t = doc.tokens[i];
      console.log(`${i}: text="${t.text}" lemma="${t.lemma}" pos=${t.pos} head=${t.head} dep=${t.dep}`);
    }
  }
}

main().catch(console.error);
