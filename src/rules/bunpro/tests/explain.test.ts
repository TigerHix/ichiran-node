import { describe, it, expect } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { BUNPRO_JLPT3 } from '../jlpt3/index.js';

describe('explainMatch', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);

  it('returns success for matching rules', async () => {
    const result = await engine.get().explainMatch('じゃあ、行きましょう。', 'では-それでは-じゃあ');
    expect(result.matched).toBe(true);
    if (result.matched) {
      expect(result.captures).toBeDefined();
    }
  });

  it('returns failure info for non-matching rules', async () => {
    const result = await engine.get().explainMatch('今日は暑いですね。', 'では-それでは-じゃあ');
    expect(result.matched).toBe(false);
    if (!result.matched) {
      expect(result.reason).toBeDefined();
      expect(typeof result.reason).toBe('string');
    }
  });
});
