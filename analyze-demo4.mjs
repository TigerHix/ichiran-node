import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const sentences = [
  '退院しても、まだ車椅子を使わなくてはいけない。',  // Positive: ても (even if)
  'これは明日までに終わらなくてもいいから。',        // Positive: なくてもいい (even if not OK)
  '行ってもいいです。',                                // Negative: てもいい (permission)
  '食べてもいいですよ。',                              // Negative: てもいい (permission)
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('Sentence:', sent);
  console.log('='.repeat(80));

  const doc = await engine.analyze(sent);
  const tokens = doc.sentences[0].tokens;

  // Find て or も
  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    if (t.text === 'て' || t.text === 'も' || t.text === 'いい') {
      console.log(`[${i}] "${t.text}" (lemma=${t.lemma}, pos=${t.pos}, dep=${t.dep}, head=${t.head}, inflectionForm=${t.inflectionForm})`);
    }
  }

  // Show full parse around the ても pattern
  console.log('\nFull parse:');
  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    console.log(`[${i}] "${t.text}" lemma=${t.lemma} pos=${t.pos} dep=${t.dep} head=${t.head}`);
  }
}
