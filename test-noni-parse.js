import { describe, test, expect } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const engine = useSharedEngine([]); // Empty ruleset for parsing only

async function analyze(sentence) {
  const doc = await engine.analyze(sentence);
  console.log('\n=== ' + sentence + ' ===');
  doc.tokens.forEach(t => {
    console.log(`${t.text} [${t.pos}] lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
  });
}

await analyze('彼は忙しいのに、ゲームをしている。');
await analyze('天気予報は悪かったのに、晴れた。');
await analyze('彼はイケメンなのにいつも汗臭い。');
await analyze('無理って言ったのにまだやるの？');
