import { GrammarEngine } from './dist/program.js';
import { GinzaClient } from './dist/ginza/client.js';
import rule from './dist/rules/bunpro/jlpt5/verb-にいく.js';

async function test() {
  const client = new GinzaClient();
  await client.start();
  const engine = await GrammarEngine.create([rule], { client });

  const testCases = [
    'たべにいく。',
    'たべにいきます。',
  ];

  for (const testCase of testCases) {
    const result = engine.analyze(testCase);
    const matches = result.grammar?.['verb-にいく'] || [];
    console.log(`\n=== ${testCase} ===`);
    console.log(`Matches: ${matches.length}`);
    if (matches.length > 0) {
      matches.forEach(m => {
        console.log(`  - "${m.text}" at [${m.start}:${m.end}]`);
      });
    }
  }

  await client.stop();
}

test().catch(console.error);
