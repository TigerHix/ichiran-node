import { describe } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT4 } from './packages/grammar/src/rules/bunpro/jlpt4/index.js';

describe('Debug', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  
  test('check tokens', async () => {
    const e = engine.get();
    const result = await e.analyze('この猫はまん丸でかわいい。');
    console.log('Tokens:');
    result.tokens.forEach(t => {
      console.log(`  text="${t.text}" pos=${t.pos} lemma=${t.lemma}`);
    });
  });
});
