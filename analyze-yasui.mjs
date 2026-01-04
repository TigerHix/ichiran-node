import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const sent = '安い電子レンジでも弁当は温められます。';
console.log('Sentence:', sent);

const doc = await engine.analyze(sent);
const tokens = doc.sentences[0].tokens;

console.log('\nFull parse:');
for (let i = 0; i < tokens.length; i++) {
  const t = tokens[i];
  console.log(`[${i}] "${t.text}" lemma=${t.lemma} pos=${t.pos} dep=${t.dep} head=${t.head} inflectionForm=${t.inflectionForm}`);
}

// Find でも
for (let i = 0; i < tokens.length; i++) {
  const t = tokens[i];
  if (t.text === 'で' || t.text === 'も') {
    console.log(`\nToken [${i}] "${t.text}":`);
    console.log(`  lemma=${t.lemma}, pos=${t.pos}, dep=${t.dep}, head=${t.head}, inflectionForm=${t.inflectionForm}`);
    const start = Math.max(0, i - 3);
    const end = Math.min(tokens.length, i + 2);
    for (let j = start; j < end; j++) {
      const tok = tokens[j];
      console.log(`    [${j}] "${tok.text}" lemma=${tok.lemma} pos=${tok.pos} dep=${tok.dep} head=${tok.head}`);
    }
  }
}
