import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const sentences = [
  // Positive: でも as "even if"
  '彼女は仕事が大変でも諦めません。',
  '安い電子レンジでも弁当は温められます。',

  // Negative: でも as "but"
  'すみません、でも無理です。',
  '雨が降っている。でも、行きます。',

  // Negative: でも after question words
  '何でも食べます。',
  'どこでもいいです。',

  // Negative: てもいい (permission)
  '行ってもいいです。',
  '食べてもいいですよ。',
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('Sentence:', sent);
  console.log('='.repeat(80));

  const doc = await engine.analyze(sent);

  // Find tokens that contain も
  for (let i = 0; i < doc.tokens.length; i++) {
    const t = doc.tokens[i];
    if (t.text === 'も' || t.text === 'でも') {
      console.log(`Token ${i}: "${t.text}" (lemma=${t.lemma}, pos=${t.pos}, dep=${t.dep}, head=${t.head})`);
      // Show context
      const start = Math.max(0, i - 2);
      const end = Math.min(doc.tokens.length, i + 3);
      console.log('  Context:');
      for (let j = start; j < end; j++) {
        const tok = doc.tokens[j];
        console.log(`    [${j}] "${tok.text}" (lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, head=${tok.head})`);
      }
    }
  }
}
