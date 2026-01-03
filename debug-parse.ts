import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([]);

  const tests = [
    '猫だ。',
    '学生だ。',
    '綺麗だ。',
    '危険だ。',
    '大切だ。',
  ];

  for (const text of tests) {
    console.log('\n=== ' + text + ' ===');
    const doc = await engine.analyze(text);
    if (doc) {
      for (const sent of doc.sentences) {
        for (let i = 0; i < sent.tokens.length; i++) {
          const t = sent.tokens[i];
          console.log(`  [${i}] text="${t.text}" lemma="${t.lemma}" pos="${t.pos}" dep="${t.dep}" head=${t.head}`);
        }
      }
    }
  }

  await engine.close();
}

main().catch(console.error);
