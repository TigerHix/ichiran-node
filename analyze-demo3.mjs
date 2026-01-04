import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const sentences = [
  'すみません、でも無理です。',
  '雨が降っている。でも、行きます。',
  '何でも食べます。',
  'どこでもいいです。',
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('Sentence:', sent);
  console.log('='.repeat(80));

  const doc = await engine.analyze(sent);

  for (let i = 0; i < doc.sentences[0].tokens.length; i++) {
    const t = doc.sentences[0].tokens[i];
    if (t.text === 'でも' || t.text === 'も') {
      console.log(`[${i}] "${t.text}" (lemma=${t.lemma}, pos=${t.pos}, dep=${t.dep}, head=${t.head}, inflectionForm=${t.inflectionForm})`);
      // Show surrounding tokens
      const start = Math.max(0, i - 2);
      const end = Math.min(doc.sentences[0].tokens.length, i + 2);
      for (let j = start; j < end; j++) {
        const tok = doc.sentences[0].tokens[j];
        console.log(`    [${j}] "${tok.text}" (lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, head=${tok.head})`);
      }
    }
  }
}
