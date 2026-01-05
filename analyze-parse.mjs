import { GrammarEngine } from './packages/grammar/src/program.js';
import rule from './packages/grammar/src/rules/bunpro/jlpt2/ていては.ts';

async function main() {
  const engine = await GrammarEngine.create([{ id: 'test', rules: [rule] }]);
  
  const sent = 'そんな歩き方をしていては、ペンギンかと思われますよ。';
  console.log('=== Analyzing:', sent, '===\n');
  
  const explain = await engine.explainMatch(sent, 'ていては');
  console.log('Matched:', explain.matched);
  console.log('Reason:', explain.reason);
  console.log('Partial bindings:', JSON.stringify(explain.partialBinding, null, 2));
  
  console.log('\n=== Full tokens ===');
  const doc = await engine.analyze(sent);
  console.log(JSON.stringify(doc.tokens.map((t, i) => ({
    i,
    text: t.text,
    lemma: t.lemma,
    pos: t.pos,
    head: t.head,
    dep: t.dep
  })), null, 2));
}

main().catch(console.error);
