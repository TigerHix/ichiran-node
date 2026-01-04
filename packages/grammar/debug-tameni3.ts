import { describe } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/jlpt3/index.js';

describe('DEBUG: ために token analysis', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);

  test('analyze 1', async () => {
    const doc = await engine.analyze('コーヒーを飲みすぎたために眠れない。');
    console.log('Sentence 1:');
    for (const t of doc.tokens) console.log(JSON.stringify(t));
  });

  test('analyze 2', async () => {
    const doc = await engine.analyze('寒いために、車が故障してしまった。');
    console.log('Sentence 2:');
    for (const t of doc.tokens) console.log(JSON.stringify(t));
  });
});
