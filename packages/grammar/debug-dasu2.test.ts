import { describe, it } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

describe('DEBUG: Analyze だす parses', () => {
  const engine = useSharedEngine([]);

  const sentences = [
    '乾杯と言わないで、のみだした。',
    '乾杯と言わないで、飲み出した。',
    'のみだした',
    '飲み出した',
  ];

  for (const s of sentences) {
    it(`analyze: ${s}`, async () => {
      const eng = engine.get();
      const doc = await eng['client'].analyze(s);
      console.log('\n=== ' + s + ' ===');
      console.log(JSON.stringify(doc, null, 2));
    });
  }
});
