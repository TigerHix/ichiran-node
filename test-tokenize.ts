import { GrammarEngine } from '/home/tiger/ichiran-node/packages/grammar/dist/program.js';

async function test() {
  const engine = new GrammarEngine([]);
  const sentence = '赤ちゃんが泣き止んだかとおもったら、また大声で泣き始めた。';
  
  // Try to get matches to see what tokens exist
  const hits = await engine.match(sentence);
  
  console.log('Total hits:', hits.length);
  for (const hit of hits) {
    console.log(`  Rule: ${hit.ruleId}`);
    console.log(`  Captures:`, JSON.stringify(hit.captures, null, 2));
  }
}

test().catch(console.error);
