import { describe, test } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/jlpt3/index.js';

describe('DEBUG: ために token analysis', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);

  test('analyze sentence with verb+tameni', async () => {
    const doc = await engine.analyze('コーヒーを飲みすぎたために眠れない。');
    console.log('\n=== コーヒーを飲みすぎたために眠れない。 ===');
    for (const t of doc.tokens) {
      if (t.text.includes('ため') || t.text === 'に') {
        console.log(JSON.stringify(t));
      }
    }
  });

  test('analyze sentence with adj+tameni', async () => {
    const doc = await engine.analyze('寒いために、車が故障してしまった。');
    console.log('\n=== 寒いために、車が故障してしまった。 ===');
    for (const t of doc.tokens) {
      if (t.text.includes('ため') || t.text === 'に') {
        console.log(JSON.stringify(t));
      }
    }
  });

  test('analyze sentence with noun+tame', async () => {
    const doc = await engine.analyze('大雨のため、サッカーの試合を中止します。');
    console.log('\n=== 大雨のため、サッカーの試合を中止します。 ===');
    for (const t of doc.tokens) {
      if (t.text.includes('た')) {
        console.log(JSON.stringify(t));
      }
    }
  });
});
