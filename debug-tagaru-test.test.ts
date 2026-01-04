import { describe, test } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

describe('Debug: Analyze たがる parsing', () => {
  const engine = useSharedEngine([]);

  test('parse sentence with たがる', async () => {
    const e = engine.get();
    const doc = await e.analyze('彼は動物園に行きたがる。');
    console.log(JSON.stringify(doc, null, 2));
  });

  test('parse sentence with たがった', async () => {
    const e = engine.get();
    const doc = await e.analyze('リサがあのサンドイッチを見て、食べたがった。');
    console.log(JSON.stringify(doc, null, 2));
  });
});
