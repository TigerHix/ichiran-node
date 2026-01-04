import { GrammarEngine } from './packages/grammar/src/program.ts';

async function main() {
  const engine = await GrammarEngine.create([]);

  const negatives = [
    'そんなこと言わないで。',
    'そこに行きたい。',
    'そんな人はいない。',
  ];

  for (const sentence of negatives) {
    console.log(`\nTesting negative: ${sentence}`);
    const hits = await engine.match(sentence);
    const sonnaniHit = hits.find(h => h.ruleId === 'そんなに');
    if (sonnaniHit) {
      console.log(`  ❌ FALSE POSITIVE: Matched with capture:`, sonnaniHit.captures);
    } else {
      console.log(`  ✓ Correctly did not match`);
    }
  }

  await engine.close();
}

main().catch(console.error);
