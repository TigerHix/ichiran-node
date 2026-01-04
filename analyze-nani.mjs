import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const sentences = [
  '安い電子レンジでも弁当は温められます。',  // Positive: even with X
  '何でも食べます。',                          // Negative: anything
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('Sentence:', sent);
  console.log('='.repeat(80));

  const doc = await engine.analyze(sent);
  const tokens = doc.sentences[0].tokens;

  // Find で and も
  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    if (t.text === 'で' || t.text === 'も') {
      console.log(`\n[${i}] "${t.text}" (lemma=${t.lemma}, pos=${t.pos}, dep=${t.dep}, head=${t.head})`);
      // Check what the head points to
      if (t.head >= 0) {
        const headTok = tokens[t.head];
        console.log(`  -> head [${t.head}] "${headTok.text}" (pos=${headTok.pos}, tag=${headTok.tag})`);
      }
    }
  }

  // Show children relationships
  console.log('\nDependency structure:');
  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    const children = tokens.filter(tok => tok.head === i);
    if (children.length > 0) {
      console.log(`[${i}] "${t.text}" has children: ${children.map(c => `"${c.text}"`).join(', ')}`);
    }
  }
}
