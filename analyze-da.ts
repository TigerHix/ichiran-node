import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const engine = await GrammarEngine.create([]);

  // Test positive examples
  const tests = [
    '猫だ。',
    '学生だ。',
    '綺麗だ。',
    '危険だ。',
    '大きいだ。',  // Should NOT match - i-adjective
    'これはペンだ。',  // Should match
    '私だ。',  // Should match
    '私だよ。',  // Should NOT match - not sentence-final
    '同じだ。',  // Should match
  ];

  for (const text of tests) {
    console.log('\n=== ' + text + ' ===');
    const doc = await engine.analyze(text);
    if (doc) {
      for (const sent of doc.sentences) {
        console.log(JSON.stringify(sent, null, 2));
      }
    }
    await engine.close();
  }
}

main().catch(console.error);
