import { describe } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/jlpt3/index.js';

describe('DEBUG: ために', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);

  test('analyze sentence', async () => {
    const doc = await engine.analyze('コーヒーを飲みすぎたために眠れない。');
    console.log(JSON.stringify(doc, null, 2));
  });
});
